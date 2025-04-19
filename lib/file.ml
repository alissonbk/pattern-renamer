

let read_dir dir =
  let rec loop lst = function
    | [] -> lst
    | f :: fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop lst
    | f :: fs -> loop (f :: lst) fs    
  in
    loop [] [dir]
    

let read_file_tree () = Sys.getcwd () |> read_dir

(* run file --mime and checks for "charset=binary"; *)
let is_binary fname = 
  let cmd = "file --mime " ^ fname in
  Utils.run_cmd cmd |> String.split_on_char '=' |>
    function 
      | _ :: h2 :: [] -> (String.trim h2) = "binary" 
      | _ -> Printf.printf "invalid file --mime result for file : %s\n" fname; false


(* FIXME: calling is_binary many times can cause performance issues as it is running a shell command every time
  to fix this, file --mime can be called once to many files, but would be necessary a better lookup algorithm to check all the output at once*)
let should_ignore (args: Types.command_args) fname  = 
  let exception Found in
  if is_binary fname then ( true ) else
  try
    List.iter (fun s -> String.split_on_char '/' s |> List.mem fname |> fun ismem -> if ismem then (raise Found) ) args.ignore;
    false 
  with
    | Found -> true
  