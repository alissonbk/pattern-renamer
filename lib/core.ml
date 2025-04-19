open Printf


let clean_up_args (args : Types.command_args) : Types.command_args =    
  let rec clean_lst lst new_list = 
    match lst with
      | [] -> new_list
      | h :: t when (String.trim h) = "" -> clean_lst t new_list
      | h :: t -> clean_lst t (String.trim h :: new_list)
  in      
 {
    recursive = args.recursive;
    ignore = args.ignore;
    multiple_from = ( clean_lst args.multiple_from []);
    multiple_to = (clean_lst args.multiple_to []);
    from_word = String.trim args.from_word;
    to_word = String.trim args.to_word
  }

let validate_args (args : Types.command_args) : bool =     
  let exception Invalid of string in

  try    
    if Utils.empty args.multiple_to && Utils.empty args.multiple_from && args.to_word = "" && args.from_word = "" 
    then (raise (Invalid "there is no words to be changed")) 
    else
    match args.from_word with
      | fw when fw <> "" && Utils.not_empty args.multiple_to || Utils.not_empty args.multiple_from || args.to_word = "" ->
        raise (Invalid "when using positional arg from_word, only use the positional arg to_word")
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
    | Invalid s -> printf "invalid args: %s" s; false  


    
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

(* let validate_patterns (wpl : Types.word_pattern list) = 
  let val_underscore (s : string Types.extra_pattern_type) = 
    String.contains (Utils.unbox_extp s) '_' 
  in
  let rec validate (main_lst: Types.word_pattern list) valid_lst invalid_lst =
    match main_lst with
      | [] -> if List.length invalid_lst > 0 then (
        printf "these invalid patterns were found and will be ignored: %s" (Utils.lst_to_string @@ List.map (fun v -> Utils.unbox_wp v) invalid_lst)
      );
      valid_lst
      | h :: t -> 
        (match h with
          | Types.Underscore s ->             
            if val_underscore s then validate t (h :: valid_lst) invalid_lst else validate t valid_lst (h :: invalid_lst)
          | _ -> failwith "todo"      
          (* TODO    *)
        )
  in
  validate wpl [] [] *)


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
    | _ -> failwith "not implemented" 
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


(* TODO: remove non core functions from core module *)
let replace_strline (from_keyword: Types.word_pattern) (to_keyword: Types.word_pattern) strline = 
  match from_keyword with
      | Underscore v1 ->                 
        (match to_keyword with 
          | Underscore v2 -> Utils.replace_substring strline (v1 |> Utils.unbox_extp) (v2 |> Utils.unbox_extp)
          | _ -> failwith "didnt match Underscore with expected type"
        )
      | CamelCase v1 ->
        (match to_keyword with 
          | CamelCase v2 -> Utils.replace_substring strline v1 v2
          | _ -> failwith "didnt match CamelCase with expected type"
        )
      | CapitalizedCamelCase v1 ->
        (match to_keyword with 
          | CapitalizedCamelCase v2 -> Utils.replace_substring strline v1 v2
          | _ -> failwith "didnt match CapitalizedCamelCase with expected type"
        )
      | SpaceSeparated v1 -> 
        (match to_keyword with 
          | SpaceSeparated v2 -> Utils.replace_substring strline (v1 |> Utils.unbox_extp) (v2 |> Utils.unbox_extp)
          | _ -> failwith "didnt match SpaceSeparated with expected type"
        )
      | _ -> strline

(* TODO: remove non core functions from core module *)
let write_tmp_files f_name (all_patterns: Types.all_patterns) =
  let fin = open_in f_name in
  let tmp = open_out @@ f_name ^ ".tmp" in            
  let rec search_and_replace (str_line: string) (pl_from: Types.word_pattern list) (pl_to: Types.word_pattern list) =
    match (pl_from, pl_to) with
      | ([], []) -> fprintf tmp "%s\n" str_line
      | ((hfrom :: tfrom), (hto :: tto)) ->  
        let replaced = replace_strline hfrom hto str_line in        
        search_and_replace replaced tfrom tto
      | _ -> failwith "lists are out of order"                
  in
  let rec loop_all_patterns tpl str_line = 
    match tpl with
        | ([], []) -> ()
        | (from :: tfrom), (to_ :: tto) ->
          search_and_replace str_line from to_;
          loop_all_patterns (tfrom, tto) str_line
        | _ -> failwith "invalid \"all patterns\" size"
  in
  let rec loop_all_file () =    
    flush_all ();
    match input_line fin with      
      | s -> 
        loop_all_patterns (all_patterns.from_lst, all_patterns.to_lst) s;
        loop_all_file ()
  in
  try  
    loop_all_file ()
  with
    | End_of_file -> printf "finished writing temporary file...\n"
    

(* relly on the "from" and to "list" be both in order *)
let temporary_replace_matches args file_list (all_patterns: Types.all_patterns) =
  ignore args;
  Utils.print_patterns all_patterns;  
  let rec loop_files = function 
    | [] -> printf "finished writting all temporary files...\n"      
    | file :: t -> 
      write_tmp_files file all_patterns; 
      loop_files t
  in
  loop_files file_list

let display_nd_confirm_changes flist () =
  let rec ask_changes lst accepted_lst =
    match lst with
      | [] -> accepted_lst      
      | h :: t ->         
        let cmd = "diff -ZBb --color=always " ^ h ^ " " ^ h ^ ".tmp" in
        printf "executing %s\n" cmd;        
        let output = Utils.run_cmd cmd in
        output |> printf "output: %s\n";        
        if String.trim output = "" then ask_changes t accepted_lst 
        else (
          printf "accept changes (Y/n)?";          
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
        printf "failed to remove file: %s\n" msg;
        clean_up_fs t
    


let run_steps args =
  let args = clean_up_args args in
  let valid = validate_args args in
  if not valid then ( printf "some arg(s) are invalid! \n" ) else

  let flow_t = discover_flow_type args in
  Utils.print_flow_type flow_t;
  let from_in_anchor_type = to_underscore args.from_word args.multiple_from flow_t in
  let to_in_anchor_type = to_underscore args.to_word args.multiple_to flow_t in
  from_in_anchor_type |> List.iter (fun p -> printf "%s\n" (Utils.unbox_wp p));
  to_in_anchor_type |> List.iter (fun p -> printf "%s\n" (Utils.unbox_wp p));
  let patterns = generate_patterns from_in_anchor_type to_in_anchor_type in  
  let file_list = File.read_file_tree () in
  temporary_replace_matches args file_list patterns;
  let confirmed_list = display_nd_confirm_changes file_list () in
  printf "confirmed list: \n"; List.iter (printf "%s, ") confirmed_list;
  apply_changes confirmed_list () |> ignore;  
  clean_up_fs file_list;
  ()

let entrypoint recursive ignore multiple_from multiple_to from_word to_word =
  let args : Types.command_args = {
    recursive = recursive;
    ignore = ignore;
    multiple_from = multiple_from;
    multiple_to = multiple_to;
    from_word = from_word;
    to_word = to_word
  }
  in
  Utils.print_input_args args;
  run_steps args;
  printf "\n\n"


  