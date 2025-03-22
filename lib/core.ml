open Printf


let clean_up (args: Types.command_args) : Types.command_args =    
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

let validate_args (args: Types.command_args) : bool =   
  let dif_list_size a b = List.length a <> List.length b in
  let not_empty lst = (List.length lst) > 0 in
  let empty lst = not @@ not_empty lst in
  let exception Invalid of string in

  try    
    if empty args.multiple_to && empty args.multiple_from && args.to_word = "" && args.from_word = "" 
    then (raise (Invalid "there is no words to be changed")) 
    else
    match args.from_word with
      | fw when fw <> "" && not_empty args.multiple_to || not_empty args.multiple_from || args.to_word = "" ->
        raise (Invalid "when using positional arg from_word, only use the positional arg to_word")
      | _ -> ();
    match args.multiple_from with
      | mf when empty mf -> true
      | mf when dif_list_size mf args.multiple_to ->  
        (match args.multiple_to with
          | mt when empty mt && args.to_word <> "" -> true
          | _ -> 
            raise (Invalid "when using multiple from, specify a list of multiple_to with same size or a single to_word (positional arg)")
        )
      | _ -> ();    
    true
  with
    | Invalid s -> printf "invalid args: %s" s; false  


let run_steps args =
  let args = clean_up args in
  validate_args args


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


  