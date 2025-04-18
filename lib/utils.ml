open Printf


let lst_to_string = List.fold_left (fun acc curr -> if acc = "" then curr else acc ^ ", " ^ curr) ""

let mtx_to_string = List.fold_left (fun acc curr -> acc ^ "\n\n" ^ lst_to_string curr) ""

let ignore _ = ()

let dif_list_size a b = List.length a <> List.length b

let not_empty lst = (List.length lst) > 0

let empty lst = not @@ not_empty lst

let explode s = List.init (String.length s) (String.get s)

(* can cause performance issues *)
let implode (cl : char list) = String.init (List.length cl) (fun i -> List.nth cl i)

let is_lower c = c = Char.lowercase_ascii c

let is_upper c = c = Char.uppercase_ascii c

let first_capitalized s = String.init (String.length s) (fun i -> if i = 0 then Char.uppercase_ascii s.[i] else s.[i])


let unbox_extp =    
    function
        | Types.AllCaptalized v -> v
        | Types.AllLower v -> v
        | Types.FirstCaptalized v -> v    

let unbox_wp = 
    function        
        | Types.Underscore v -> unbox_extp v
        | Types.CamelCase v -> v        
        | Types.CapitalizedCamelCase v -> v        
        | Types.Lower v -> v
        | Types.SpaceSeparated v -> unbox_extp v        
        (* | Types.Gramatical v -> unbox_extp v      *)
        | Types.InvalidPattern -> failwith "cannot unbox invalid pattern"   

let has_upper cl = 
    try 
        ignore @@ List.find (fun c -> is_upper c) cl;
        true
    with 
        | Not_found -> false

let has_lower cl = 
    try 
        ignore @@ List.find (fun c -> is_upper c) cl;
        true
    with 
        | Not_found -> false


let print_flow_type = function
        | Types.Single -> printf "Single\n"
        | Types.Multiple -> printf "Multiple\n"
        | Types.MultipleFromSingleTo -> printf "MultipleFromSingleTo\n"


let print_patterns (all_patterns: Types.all_patterns) =        
    let f p = p |> List.map (fun lst -> List.map (fun p -> 
        match p with
            | Types.Underscore v -> "Underscore: " ^ (unbox_extp v)
            | Types.CamelCase v -> "CamelCase: " ^ v
            | Types.CapitalizedCamelCase v -> "CapitalizedCamelCase: " ^ v
            | Types.SpaceSeparated v -> "SpaceSeparated: " ^ (unbox_extp v)
            | Types.Lower v -> "Lower: " ^ v
            | Types.InvalidPattern -> "invalid pattern"
        ) 
        lst) 
        |> mtx_to_string |> printf "%s \n"
    in
    f all_patterns.from_lst;
    f all_patterns.to_lst


(* replace only the pattern that was found *)
let replace_substring s sub repl =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i > len_s - len_sub then s 
    else if String.sub s i len_sub = sub then
      let before = String.sub s 0 i in
      let after = String.sub s (i + len_sub) (len_s - i - len_sub) in
      before ^ repl ^ after
    else loop (i + 1)
  in
  loop 0


let print_input_args (args : Types.command_args) =   printf "recursive: %b\nignore: %s\nmultiple_from: %s\nmultiple_to: %s\nfrom_word: %s\nto_word: %s\n" 
    args.recursive (lst_to_string args.ignore) (lst_to_string args.multiple_from) (lst_to_string args.multiple_to) args.from_word args.to_word


let run_cmd cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp; r

