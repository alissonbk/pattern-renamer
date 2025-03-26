

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