let camel_to_underscore ?(capitalized=false) s =
  if s = "" then "" else
  
  let len = String.length s in
  let buffer = Buffer.create (len * 2) in    
  let first_case_fn = (if capitalized then Char.uppercase_ascii else Char.lowercase_ascii) in  
  Buffer.add_char buffer (first_case_fn s.[0]);
  List.iteri (fun idx c -> 
    if idx = 0 then () else
    if Utils.is_lower c then (Buffer.add_char buffer c ) 
    else (
      Buffer.add_char buffer '_';
      Buffer.add_char buffer (Char.lowercase_ascii c)
    )    
    ) (Utils.explode s);
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
  let counter = ref 1 in  
  List.iter (
    fun _ -> 
      if !counter = (len) then () else
      if s.[!counter] = '_' then (
        Buffer.add_char buf (Char.uppercase_ascii s.[!counter + 1]);
        counter := !counter + 2
      ) else (
        Buffer.add_char buf s.[!counter];
        counter := !counter + 1
        )
  ) (Utils.explode s);  
  Buffer.contents buf



let to_all_extrapatterns s = 
  [
    Types.AllLower (String.lowercase_ascii s);
    Types.AllCaptalized (String.uppercase_ascii s);
    Types.FirstCaptalized (Utils.first_capitalized s)
  ]

let underscore_to_all_patterns =     
    function
    | Types.Underscore v ->   
      let unboxed_v = Utils.unbox_extp v in      
      let lst = [   
        (Types.Lower (String.lowercase_ascii unboxed_v) ); 
        (Types.CamelCase (underscore_to_camel unboxed_v));        
        (Types.CapitalizedCamelCase (underscore_to_camel ~captalized:true unboxed_v))                
      ] in
      [
        to_all_extrapatterns unboxed_v |> List.map (fun exp -> Types.Underscore exp); 
        to_all_extrapatterns @@ underscore_to_space unboxed_v
          |> List.map (fun exp -> Types.SpaceSeparated exp)
      ] |> List.flatten |> List.append lst

    | _ -> failwith "invalid pattern, it should be Types.Underscore"


  