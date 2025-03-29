

let camel_to_underscore s =
  if s = "" then "" else
    
  let len = String.length s in
  let buffer = Buffer.create (len * 2) in
  Buffer.add_char buffer (Char.lowercase_ascii s.[0]);
  for i = 1 to (len - 1) do
    let c = s.[i] in
    if Utils.is_lower c then (
      Buffer.add_char buffer '_';
      Buffer.add_char buffer (Char.lowercase_ascii c)
    )else
      Buffer.add_char buffer c
  done;
  Buffer.contents buffer


let space_to_underscore s =
  if s = "" then "" else  
  List.map (fun c -> if c = ' ' then ('_') else c) (Utils.explode s) |> Utils.implode  


(* dont change case *)
let underscore_to_space s =
  List.map (fun c -> if c = '_' then ' ' else c) (Utils.explode s) |> Utils.implode


let underscore_to_camel ?(captalized = false) s  =
  if s = "" then "" else
  let len = String.length s in
  let buf = Buffer.create len in
  (match captalized with
    | true -> Buffer.add_char buf (Char.uppercase_ascii s.[0])
    | false -> Buffer.add_char buf (Char.lowercase_ascii s.[0]));
  let counter = ref 0 in
  List.iter (
    fun _ -> 
      if s.[!counter] = '_' then (
        Buffer.add_char buf (Char.uppercase_ascii s.[!counter + 1]);
        counter := !counter + 2
      ) else (
        Buffer.add_char buf s.[!counter];
        counter := !counter + 1
        )
  ) (Utils.explode s);
  Buffer.contents buf


  