

type command_args = {
  recursive : bool;
  ignore_files : string list;
  ignore_patterns : string list;  
  yes : bool;
  from_words : string list;
  to_words : string list;
  debug_mode : bool;
}


(* can be combined with all writting_pattern's ()*)
type 't extra_pattern_type =
  | AllLower of 't
  (* the others after space for instance could also be capitalized, but this is ignored for now *)
  | FirstCaptalized of 't
  | AllCaptalized of 't  

type word_pattern =
  (* some_example, ** Underscore is the anchor type for doing transformation ** *)
  | Underscore of string extra_pattern_type
  (* someExample *)
  | CamelCase of string 
  (* SomeExample *)
  | CapitalizedCamelCase of string    
  (* some example | Some example | Some Example*)
  | SpaceSeparated of string extra_pattern_type
  (* someexample *)
  | Lower of string
  (* Gramatical is useful for languages with accent like Portuguese (gramática -> grammar)*)
  (* | Gramatical of string extra_pattern_type *)
  | InvalidPattern

type all_patterns = {
  from_lst : word_pattern list list;
  to_lst : word_pattern list list;
}  

(* FIXME: needs a better name
type represents a word that was found and is probably a change candidate*)
type word_match = {
  file_path : string;
  pattern : word_pattern  
}
  
type flow_type =  
  | MultipleFromSingleTo
  | Multiple

type flush_t = 
  | None
  | Stdout 
  | All   

type log_level = 
  | Success
  | Ask  
  | Info 
  | Warning 
  | Error
  | Debug of command_args