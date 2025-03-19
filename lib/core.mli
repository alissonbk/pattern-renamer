open Types


(* entrypoint called from the command.ml *)
val entrypoint : bool -> string list -> string list -> string list -> string -> string -> unit

(* Divided in a few steps 
  -> Validate 
  -> Generate all patterns
  -> Search for matchings 
  -> Display changes (before than after), asking for confirm (y/n)
  -> Apply changes (if accepted)
*)
val run_all_steps : command_args -> unit

val validate_args : command_args -> bool


(* a list of all writting_patterns for each word in the args *)
val generate_patterns : command_args -> word_pattern list list

(* find all matches *)
val search_matchings : command_args -> word_pattern list list -> word_match list

(* display the changes before apply and asks y/n ** has side effects ** *)
val display_changable_items : word_match list -> unit -> bool

(* write changes to disk and return bool representing if it was successful *)
val apply_changes : unit -> bool

