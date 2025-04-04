open Printf


let lst_to_string = List.fold_left (fun acc curr -> acc ^ "," ^ curr) ""

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
        | Types.Gramatical v -> unbox_extp v     
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

let print_input_args (args : Types.command_args) =   printf "recursive: %b\nignore: %s\nmultiple_from: %s\nmultiple_to: %s\nfrom_word: %s\nto_word: %s\n" 
    args.recursive (lst_to_string args.ignore) (lst_to_string args.multiple_from) (lst_to_string args.multiple_to) args.from_word args.to_word;