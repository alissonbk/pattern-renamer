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

(* let to_underscore p = () *)




  (* type word_pattern =
  (* some_example *)
  | Underscore of string
  (* someExample *)
  | CamelCase of string 
  (* SomeExample *)
  | CapitalizedCamelCase of string  
  (* some example | Some example | Some Example*)
  | SpaceSeparated of string extra_pattern_type
  (* Gramatical is useful for languages with accent like Portuguese (gramática -> grammar)*)
  | Gramatical of string extra_pattern_type  *)
(* let generate_patterns (args : Types.command_args) (flow_type : Types.flow_type) =
  let gen_underscode s = 
  [] *)



let run_steps args =
  let args = clean_up args in
  let valid = validate_args args in
  let flow_t = discover_flow_type args in
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
  printf "\n"


  