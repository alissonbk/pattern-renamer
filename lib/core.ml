open Printf


let clean_up (args : Types.command_args) : Types.command_args =    
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
let to_underscore (args : Types.command_args) (flow_type : Types.flow_type) =
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
    | Single -> [transform @@ identify_pattern args.from_word]
    | MultipleFromSingleTo 
    | Multiple -> List.map (fun e -> transform @@ identify_pattern e) args.multiple_from



(* *)  
let generate_patterns (pattern_list: Types.word_pattern list) =  
  let rec loop new_list = function
      | [] -> new_list
      | h :: t -> 
        loop (Transform.underscore_to_all_patterns h :: new_list) t
  in loop [] pattern_list



let temporary_write_file_changes f_name =
  let fin = open_in f_name in
  let tmp = open_out f_name in
  try
    match input_line fin with
      | "" -> ()
      | s -> ()
        (* todo replace substring, need to handle multiple *)
        
  with
    | End_of_file -> ()
    


let search_matchings args all_patterns =
  ignore args;
  Utils.print_patterns all_patterns;
  let file_list = Sys.getcwd () |> File.read_dir in
  file_list |> List.iter (printf "%s\n")
  let rec loop_files = function 
    | [] -> printf "finished writting to temporary files\n"      
    | file :: t -> temporary_write_file_changes f; loop_files t
  in
  loop_files file_list



let run_steps args =
  let args = clean_up args in
  let valid = validate_args args in
  if not valid then ( printf "some arg(s) are invalid! \n" ) else

  let flow_t = discover_flow_type args in
  Utils.print_flow_type flow_t;
  let all_in_anchor_type = to_underscore args flow_t in
  all_in_anchor_type |> List.iter (fun p -> printf "%s\n" (Utils.unbox_wp p));
  let patterns = generate_patterns all_in_anchor_type in
  search_matchings args patterns;
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
  printf "\n"


  