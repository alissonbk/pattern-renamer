open Types


(* entrypoint called from the command.ml *)
val entrypoint : bool -> string list -> string list -> string list -> string -> string -> unit

(* Divided in a few steps 
  -> Clean up
  -> Validate 
  -> Generate all patterns
  -> Search for matchings 
  -> Display changes (before than after), asking for confirm (y/n)
  -> Apply changes (if accepted)
*)
val run_steps : command_args -> unit

val clean_up : command_args -> command_args

val validate_args : command_args -> bool

(* considering args validated, discover which flow is it doing based on args, 
could be done in the validate_args fun., but would mix things up *)
val discover_flow_type : command_args -> flow_type

(* a list of all writting_patterns for each word in the args *)
val generate_patterns : command_args -> word_pattern list list

(* find all matches *)
val search_matchings : command_args -> word_pattern list list -> word_match list

(* display the changes before apply and asks y/n ** has side effects ** *)
val display_changable_items : word_match list -> unit -> bool

(* write changes to disk and return bool representing if it was successful *)
val apply_changes : unit -> bool

val identify_pattern : string -> word_pattern

val identify_extra_pattern : string -> string extra_pattern_type

(* the application needs a anchor pattern to makes things easier, so this function transform any other pattern to underscore*)
val to_underscore : word_pattern -> word_pattern

