

let clean_up_args (args : Types.command_args) : Types.command_args =    
  let rec clean_lst lst new_list = 
    match lst with
      | [] -> new_list
      | h :: t when (String.trim h) = "" -> clean_lst t new_list
      | h :: t -> clean_lst t (String.trim h :: new_list)
  in      
 {
    recursive = args.recursive;
    ignore_files = args.ignore_files;
    ignore_patterns = args.ignore_patterns;
    from_words = ( clean_lst args.from_words []);
    to_words = (clean_lst args.to_words []);    
    debug_mode = args.debug_mode
  }

(* TODO : IMPROVE THIS *)
let validate_args (args : Types.command_args) : bool =     
  let exception Invalid of string in
  try    
    if Utils.empty args.to_words && Utils.empty args.from_words
    then (raise (Invalid "there is no words to be changed")) 
    else        
    true
  with
    | Invalid s -> Log.log Error @@ "invalid args: " ^ s; false  


    
let discover_flow_type : Types.flow_type =  
  if Utils.not_empty Global.args.from_words && Utils.empty Global.args.to_words then (
    Types.MultipleFromSingleTo
  ) else if Utils.not_empty Global.args.from_words && Utils.not_empty Global.args.to_words then (
    Types.Multiple
  ) else  (
    let msg = "could not discover_flow_type" in 
    Log.log Error msg;
    failwith msg
  )


let identify_extra_pattern s =
  let has_upper = Utils.has_upper @@ Utils.explode s in
  let has_lower = Utils.has_lower @@ Utils.explode s in
  if has_upper then (
    if has_lower then ( Types.FirstCaptalized s ) else Types.AllCaptalized s
  )
  else Types.AllLower s
  

(* missing a pattern validation *)
let identify_pattern s =
  let split = Utils.explode s in
  let first_lower = Utils.is_lower @@ List.hd split in
  let first_upper = Utils.is_upper @@ List.hd split in
  let has_underscode = String.exists (fun c -> c = '_') s in
  let has_space = String.exists (fun c -> c = ' ') s in    
  match () with
    | _ when has_underscode ->
      Types.Underscore (identify_extra_pattern s)
    | _ when has_space ->
      Types.SpaceSeparated (identify_extra_pattern s)
    | _ when first_lower ->
      if Utils.has_upper @@ List.tl split then (
        Types.CamelCase s
      ) else Types.Lower s
    | _ when first_upper ->
      if Utils.has_lower split && Utils.has_upper @@ List.tl split then (
        Types.CapitalizedCamelCase s
      ) else InvalidPattern
    | _ -> InvalidPattern


(* identify and transform to underscore *)
let to_underscore words (flow_type : Types.flow_type) =
  let transform = function
    | Types.Underscore v -> Types.Underscore v
    | Types.CamelCase v -> Types.Underscore (Types.AllLower (Transform.camel_to_underscore v))
    | Types.CapitalizedCamelCase v -> Types.Underscore (Types.AllLower (Transform.camel_to_underscore ~capitalized: true v))
    | Types.Lower v -> Types.Underscore (Types.AllLower v)
    | Types.SpaceSeparated v -> 
      let s = Utils.unbox_extp v in
      Types.Underscore (Types.AllLower (Transform.space_to_underscore s))
    | _ -> Log.log_nd_fail "not implemented" 
  in
  match flow_type with     
    | MultipleFromSingleTo 
    | Multiple -> List.map (fun e -> transform @@ identify_pattern e) words



(* *)  
let generate_patterns (from_pattern_list: Types.word_pattern list) (to_pattern_list: Types.word_pattern list) : Types.all_patterns =
  let rec loop new_list = function
      | [] -> new_list
      | h :: t -> 
        loop (Transform.underscore_to_all_patterns h :: new_list) t        
  in 
  { from_lst = loop [] from_pattern_list; to_lst = loop [] to_pattern_list }


let is_ignored_pattern str_line from_token to_token ignore_patterns = 
  let exception Found in  
  let rec loop = function
    | [] -> false
    | pattern :: t ->                  
      let regexp token = Utils.replace_substring token token (".*" ^ token ^ ".*") |> Utils.replace_substring pattern "$" |> Str.regexp in      
      let contains_before = Utils.str_contains str_line (regexp from_token) in
      let replace_to = Utils.replace_substring str_line from_token to_token in
      let contains_to = Utils.str_contains replace_to (regexp to_token) in      
      if contains_before && contains_to then (raise Found);
      loop t
  in
  try 
    loop ignore_patterns    
  with 
    | Found -> true



(* TODO: remove non core functions from core module *)
let rec replace_strline (from_pattern: Types.word_pattern) (to_pattern: Types.word_pattern) (strline: string) (ignore_patterns: string list) (already_replaced: (int * int) list ref) = 
  let trigger_recursion_when_changed result =
    if result <> strline then replace_strline from_pattern to_pattern result ignore_patterns already_replaced else result
  in
  let replace from_token to_token = 
    if is_ignored_pattern strline from_token to_token ignore_patterns then (
      strline
    ) else (
      Utils.replace_substring strline from_token to_token ~already_replaced:already_replaced
    )
  in
  match from_pattern with
      | Underscore from_p ->                 
        (match to_pattern with 
          | Underscore to_p -> replace (from_p |> Utils.unbox_extp) (to_p |> Utils.unbox_extp) |> trigger_recursion_when_changed
          | _ -> Log.log_nd_fail "didnt match Underscore with expected type"
        )
      | CamelCase from_token ->
        (match to_pattern with 
          | CamelCase to_token -> replace from_token to_token |> trigger_recursion_when_changed
          | _ -> Log.log_nd_fail "didnt match CamelCase with expected type"
        )
      | CapitalizedCamelCase from_token ->
        (match to_pattern with 
          | CapitalizedCamelCase to_token -> replace from_token to_token |> trigger_recursion_when_changed
          | _ -> Log.log_nd_fail "didnt match CapitalizedCamelCase with expected type"
        )
      | SpaceSeparated from_token -> 
        (match to_pattern with 
          | SpaceSeparated to_token -> replace (from_token |> Utils.unbox_extp) (to_token |> Utils.unbox_extp) |> trigger_recursion_when_changed
          | _ -> Log.log_nd_fail "didnt match SpaceSeparated with expected type"
        )
      | _ -> strline

(* TODO: remove non core functions from core module *)
let write_tmp_files f_name (all_patterns: Types.all_patterns) ignore_patterns =
  let fin = open_in f_name in
  let tmp = open_out @@ f_name ^ ".tmp" in
  Log.log Debug @@ "started writing temporary file " ^ f_name;
  let rec replace_all_list fromlst tolst str_line already_replaced =
    match (fromlst, tolst) with
      | ([], []) -> str_line
      | (hfrom :: tfrom), (hto :: tto) ->
        let replaced = replace_strline hfrom hto str_line ignore_patterns already_replaced in
        replace_all_list tfrom tto replaced already_replaced
      | _ -> Log.log_nd_fail "lists are out of order"
  in
  let rec search_and_replace str_line (pl_from: Types.word_pattern list list) (pl_to: Types.word_pattern list list) already_replaced =
    match (pl_from, pl_to) with
      | ([], []) -> Printf.fprintf tmp "%s\n" str_line
      | ((hfrom :: tfrom), (hto :: tto)) ->  
        let replaced = replace_all_list hfrom hto str_line already_replaced in        
        search_and_replace replaced tfrom tto already_replaced
      | _ ->       
        Log.log_nd_fail "lists are out of order"
  in
  let rec loop_file () =    
    flush_all ();    
    match input_line fin with      
      | s ->         
        search_and_replace s all_patterns.from_lst all_patterns.to_lst (ref []);
        loop_file ()
  in
  try  
    loop_file ()
  with
    | End_of_file -> Log.log Debug "finished writing temporary file..."
    

(* relly on the "from" and to "list" be both in order *)
let temporary_replace_matches file_list (all_patterns: Types.all_patterns) ignore_patterns =  
  let rec loop_files = function 
    | [] -> Log.log Success "finished generating all temporary files...\n"      
    | file :: t -> 
      write_tmp_files file all_patterns ignore_patterns; 
      loop_files t
  in
  loop_files file_list

let display_nd_confirm_changes flist () =
  let rec ask_changes lst accepted_lst =
    match lst with
      | [] -> accepted_lst      
      | h :: t ->         
        let cmd = "diff -ZBb --color=always " ^ h ^ " " ^ h ^ ".tmp" in
        Log.log Debug @@ "executing " ^ cmd;                
        let diff_output = Utils.run_cmd cmd in
        let changes = String.trim diff_output <> "" in        
        if changes then Log.log Info @@ "changes in file " ^ h;
        Log.log Debug @@ "cmd output: " ^ diff_output;
        if not changes then ask_changes t accepted_lst 
        else (
          Log.log Ask "accept changes (Y/n)?";
          flush_all ();
          let r = Scanf.scanf "%s\n" (fun x -> String.lowercase_ascii x) in
          if r = "" || r = "y" then ask_changes t (h :: accepted_lst) 
          else ask_changes t accepted_lst
        )        
        
  in
  ask_changes flist []


let apply_changes confirmed_list () =
  let rec apply_all = function
    | [] -> true
    | h :: t -> 
      let cmd = "mv " ^ h ^ ".tmp" ^ " " ^ h in
      Utils.run_cmd cmd |> ignore;
      apply_all t
  in
  apply_all confirmed_list


    


let run_steps =
  Global.set_args_state @@ clean_up_args Global.args;
  let valid = validate_args Global.args in
  if not valid then ( Log.log Warning "some arg(s) are invalid!" ) else

  let flow_t = discover_flow_type in
  Log.log_flow_type flow_t;
  let from_in_anchor_type = to_underscore Global.args.from_words flow_t in
  let to_in_anchor_type = to_underscore Global.args.to_words flow_t in  
  Log.log Debug "\"From\" in anchor type:"; from_in_anchor_type |> List.iter (fun e -> Log.log Debug (Utils.unbox_wp e));
  Log.log Debug "\"To\" in anchor type:"; to_in_anchor_type |> List.iter (fun e -> Log.log Debug (Utils.unbox_wp e));
  let patterns = generate_patterns from_in_anchor_type to_in_anchor_type in  
  Log.log_patterns patterns;  
  let file_list = File.read_file_tree () |> List.filter (fun f -> not (File.should_ignore Global.args f) ) in
  temporary_replace_matches file_list patterns Global.args.ignore_patterns;
  let confirmed_list = display_nd_confirm_changes file_list () in    
  Log.log Debug "Confirmed list: "; confirmed_list |> List.iter (fun e -> Log.log Debug e);
  apply_changes confirmed_list () |> ignore;  
  File.clean_up_fs file_list;
  ()

let entrypoint recursive ignore_files ignore_patterns from_words to_words debug_mode =  
  Global.set_args_state {
    recursive = recursive;
    ignore_files = ignore_files;
    ignore_patterns = ignore_patterns;        
    from_words = from_words;
    to_words = to_words;
    debug_mode = debug_mode
  };  
  Log.log_input_args Global.args;
  run_steps;  


  