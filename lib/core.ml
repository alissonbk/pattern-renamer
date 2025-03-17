open Printf


let run_all_steps args = Utils.ignore args

let validate_args args = Utils.ignore args; true

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


  