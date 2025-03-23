

type command_args = {
  recursive : bool;
  ignore : string list;
  multiple_from : string list;
  multiple_to : string list;
  from_word : string;
  to_word : string;
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
  | Gramatical of string extra_pattern_type
  | InvalidPattern

(* FIXME: needs a better name
type represents a word that was found and is probably a change candidate*)
type word_match = {
  file_path : string;
  pattern : word_pattern
}
  
type flow_type =
  | Single
  | MultipleFromSingleTo
  | Multiple
