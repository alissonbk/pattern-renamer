

let args_state: Types.command_args option ref = ref None
let unbind_option = function 
  | None -> failwith "cannot access field, the record is none"
  | Some args -> args

let set_args_state args = args_state := Some(args)

let args = !args_state |> unbind_option