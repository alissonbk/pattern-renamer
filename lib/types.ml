

type command_args = {
  recursive : bool;
  ignore : string list;
  multiple_from : string list;
  multiple_to : string list;
  from_word : string;
  to_word : string;
}


(* can be combined with all writting_pattern's ()*)
type 't  extra_pattern_type =
  | AllLower of 't
  | FirstCaptalized of 't
  | AllCaptalized of 't

type word_pattern =
  (* some_example *)
  | Underscore of string
  (* someExample *)
  | CamelCase of string 
  (* SomeExample *)
  | CapitalizedCamelCase of string  
  (* some example | Some example | Some Example*)
  | SpaceSeparated of string extra_pattern_type
  (* Gramatical is useful for languages with accent like Portuguese (gramática -> grammar)*)
  | Gramatical of string extra_pattern_type 

(* type represents a word that was found and is probably a change candidate*)
type word_match = {
  file_path : string;
  pattern : word_pattern
}
