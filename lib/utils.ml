open Printf


let lst_to_string = List.fold_left (fun acc curr -> acc ^ "," ^ curr) ""

let ignore _ = ()

let print_input_args (args : Types.command_args) =   printf "recursive: %b\nignore: %s\nmultiple_from: %s\nmultiple_to: %s\nfrom_word: %s\nto_word: %s\n" 
    args.recursive (lst_to_string args.ignore) (lst_to_string args.multiple_from) (lst_to_string args.multiple_to) args.from_word args.to_word;