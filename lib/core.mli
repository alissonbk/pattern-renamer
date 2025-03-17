open Types
(* 
* Divided in few steps 
  -> Validate 
  -> Search for matchings 
  -> Display changes (before than after), asking for confirm (y/n)
  -> Write changes to all the files 
*)
val run_all_steps : command_args -> unit

val validate_args : command_args -> bool

(* entrypoint called from the command.ml *)
val entrypoint : bool -> string list -> string list -> string list -> string -> string -> unit