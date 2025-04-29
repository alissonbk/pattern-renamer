open Types


(* entrypoint called from the command.ml *)
val entrypoint : bool -> string list -> string list -> string list -> string list -> string -> string -> bool -> unit

(* Divided in a few steps 
  -> Clean up
  -> Validate args
  -> Transform to anchor pattern (Underscore) 
  -> Generate all patterns (so it can be searched)
  -> optional Validate patterns ( maybe latter with better word pattern validation)  
  -> Temporary replace matches
  -> Display changes (before than after) and asking for confirm (y/n)
  -> Apply changes
  -> Clean up fs
*)
val run_steps : command_args -> unit

val clean_up_args : command_args -> command_args

val validate_args : command_args -> bool

(* considering args validated, discover which flow is it doing based on args, 
could be done in the validate_args fun., but would mix things up *)
val discover_flow_type : command_args -> flow_type

(* the application needs a anchor pattern to makes things easier, so this function transform any other pattern to underscore*)
val to_underscore : string -> string list -> flow_type -> word_pattern list

(* a list of all writting_patterns for each word in the args *)
val generate_patterns : word_pattern list -> word_pattern list -> all_patterns

(* find all matches *)
val temporary_replace_matches : string list -> all_patterns -> bool -> unit

(* display the changes before apply and asks y/n ** has side effects ** 
  recieves a list of the file names, show diff between orig. and .tmp then returns a list with accepted files*)
val display_nd_confirm_changes : string list -> unit -> string list

(* write changes to disk and return bool representing if it was successful *)
val apply_changes : string list -> unit -> bool

val clean_up_fs : string list -> unit

