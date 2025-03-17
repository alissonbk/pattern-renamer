open Printf

(* 
* Divided in few steps 
  -> Validate 
  -> Search for matchings 
  -> Display changes (before than after), asking for confirm (y/n)
  -> Write changes to all the files 
*)

(* when contains multiple_from *)
(* let validate_lists *)

let renamer recursive ignore multiple_from multiple_to from_word to_word =
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

  