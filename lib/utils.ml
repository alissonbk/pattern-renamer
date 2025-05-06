

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


let rec is_bettwen_indexes start endd lst =
  match lst with
    | [] -> false
    | (s, e) :: _ when start >= s && endd <= e -> true
    | _ :: t -> is_bettwen_indexes start endd t

(* replace only the pattern that was found *)
let replace_substring ?(already_replaced: (int * int) list ref = ref []) s sub repl =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let len_repl = String.length repl in      
  let rec loop i =       
    if i < len_s - len_repl && String.sub s i len_repl = repl && len_repl > len_sub 
      then loop (i + 1)
    else
    if i > len_s - len_sub then s
    else       
      if is_bettwen_indexes i (i + len_sub) !already_replaced then (loop (i + len_repl))
      else
      if String.sub s i len_sub = sub then                
        let before = String.sub s 0 i in
        let after = String.sub s (i + len_sub) (len_s - i - len_sub) in                
        already_replaced := (i, i + len_repl) :: !already_replaced;
        before ^ repl ^ after
    else loop (i + 1)
  in
  loop 0


let str_contains str sub_regex =
  try
    ignore (Str.search_forward sub_regex str 0);
    true
  with Not_found -> false


let run_cmd cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp; r

