
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
    multiple_from = ( clean_lst args.multiple_from []);
    multiple_to = (clean_lst args.multiple_to []);
    from_word = String.trim args.from_word;
    to_word = String.trim args.to_word;
    debug_mode = args.debug_mode
  }

let validate_args (args : Types.command_args) : bool =     
  let exception Invalid of string in

  try    
    if Utils.empty args.multiple_to && Utils.empty args.multiple_from && args.to_word = "" && args.from_word = "" 
    then (raise (Invalid "there is no words to be changed")) 
    else
    match args.from_word with
      | fw when fw <> "" && (Utils.not_empty args.multiple_to || Utils.not_empty args.multiple_from || args.to_word = "") ->
        raise (Invalid "when using positional arg from_word, only use the positional arg to_word\n")
      | _ -> ();
    match args.multiple_from with
      | mf when Utils.empty mf -> true
      | mf when Utils.dif_list_size mf args.multiple_to ->  
        (match args.multiple_to with
          | mt when Utils.empty mt && args.to_word <> "" -> true
          | _ -> 
            raise (Invalid "when using multiple from, specify a list of multiple_to with same size or a single to_word (positional arg)")
        )
      | _ -> ();    
    true
  with
    | Invalid s -> Log.log Error @@ "invalid args: " ^ s; false  


    
let discover_flow_type (args : Types.command_args) : Types.flow_type =
  if Utils.not_empty args.multiple_from && Utils.empty args.multiple_to then (
    Types.MultipleFromSingleTo
  ) else if Utils.not_empty args.multiple_from && Utils.not_empty args.multiple_to then (
    Types.Multiple
  ) else Types.Single


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
let to_underscore from multiple_from (flow_type : Types.flow_type) =
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
    | Single -> [transform @@ identify_pattern from]
    | MultipleFromSingleTo 
    | Multiple -> List.map (fun e -> transform @@ identify_pattern e) multiple_from



(* *)  
let generate_patterns (from_pattern_list: Types.word_pattern list) (to_pattern_list: Types.word_pattern list) : Types.all_patterns =
  let rec loop new_list = function
      | [] -> new_list
      | h :: t -> 
        loop (Transform.underscore_to_all_patterns h :: new_list) t        
  in 
  { from_lst = loop [] from_pattern_list; to_lst = loop [] to_pattern_list }


let is_ignored_pattern str_line token ignore_patterns = 
  let exception Found in  
  let rec loop = function
    | [] -> false
    | pattern :: t ->                   
      let formated_token = Utils.replace_substring token token (".*" ^ token ^ ".*") in
      let regexp = Utils.replace_substring pattern "$" formated_token |> Str.regexp in  
      if Utils.str_contains str_line regexp = true then (raise Found);
      loop t
  in
  try 
    loop ignore_patterns    
  with 
    | Found -> true



(* TODO: remove non core functions from core module *)
let rec replace_strline (from_pattern: Types.word_pattern) (to_pattern: Types.word_pattern) (strline: string) (ignore_patterns: string list) = 
  let trigger_recursion_when_changed result =
    if result <> strline then replace_strline from_pattern to_pattern result ignore_patterns else result
  in 
  let replace from_token to_token = 
    if is_ignored_pattern strline from_token ignore_patterns then (
      strline
    ) else (
      Utils.replace_substring strline from_token to_token
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
  let rec replace_all_list fromlst tolst str_line =
    match (fromlst, tolst) with
      | ([], []) -> str_line
      | (hfrom :: tfrom), (hto :: tto) ->
        let replaced = replace_strline hfrom hto str_line ignore_patterns in
        replace_all_list tfrom tto replaced
      | _ -> Log.log_nd_fail "lists are out of order"
  in
  let rec search_and_replace (str_line: string) (pl_from: Types.word_pattern list list) (pl_to: Types.word_pattern list list) =
    match (pl_from, pl_to) with
      | ([], []) -> Printf.fprintf tmp "%s\n" str_line
      | ((hfrom :: tfrom), (hto :: tto)) ->  
        let replaced = replace_all_list hfrom hto str_line in        
        search_and_replace replaced tfrom tto
      | _ ->       
        Log.log_nd_fail "lists are out of order"
  in
  let rec loop_all_file () =
    flush_all ();
    match input_line fin with      
      | s -> 
        search_and_replace s all_patterns.from_lst all_patterns.to_lst;
        loop_all_file ()
  in
  try  
    loop_all_file ()
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

let rec clean_up_fs = function
  | [] -> ()
  | h :: t ->
    try      
      Sys.remove @@ h ^ ".tmp";
      clean_up_fs t
    with
      | Sys_error msg -> 
        Log.log Info @@ "failed to remove file: " ^ msg;
        clean_up_fs t
    


let run_steps args =
  let args = clean_up_args args in
  let valid = validate_args args in
  if not valid then ( Log.log Warning "some arg(s) are invalid!" ) else

  let flow_t = discover_flow_type args in
  Log.log_flow_type flow_t;
  let from_in_anchor_type = to_underscore args.from_word args.multiple_from flow_t in
  let to_in_anchor_type = to_underscore args.to_word args.multiple_to flow_t in
  if args.debug_mode then (
    Log.log Debug "\"From\" in anchor type:"; from_in_anchor_type |> List.iter (fun e -> Log.log Debug (Utils.unbox_wp e));
    Log.log Debug "\"To\" in anchor type:"; to_in_anchor_type |> List.iter (fun e -> Log.log Debug (Utils.unbox_wp e))
  );  
  let patterns = generate_patterns from_in_anchor_type to_in_anchor_type in  
  if args.debug_mode then (
    Log.log_patterns patterns
  );
  let file_list = File.read_file_tree () |> List.filter (fun f -> not (File.should_ignore args f) ) in
  temporary_replace_matches file_list patterns args.ignore_patterns;
  let confirmed_list = display_nd_confirm_changes file_list () in  
  if args.debug_mode then (
    Log.log Debug "Confirmed list: "; confirmed_list |> List.iter (fun e -> Log.log Debug e)
  );  
  apply_changes confirmed_list () |> ignore;  
  clean_up_fs file_list;
  ()

let entrypoint recursive ignore_files ignore_patterns multiple_from multiple_to from_word to_word debug_mode =
  let args : Types.command_args = {
    recursive = recursive;
    ignore_files = ignore_files;
    ignore_patterns = ignore_patterns;
    multiple_from = multiple_from;
    multiple_to = multiple_to;
    from_word = from_word;
    to_word = to_word;
    debug_mode = debug_mode
  }
  in
  if args.debug_mode then (
    Log.log_input_args args
  );
  run_steps args;  


  