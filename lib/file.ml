

let read_dir dir =
  let rec loop final_lst = function
    | [] -> final_lst
    | h :: t when Sys.is_directory h ->                  
      if Utils.str_contains h (Str.regexp ".git") then ( loop final_lst t) else
      Sys.readdir h
        |> Array.to_list
        |> List.map (Filename.concat h)
        |> List.append t
        |> loop final_lst
    | h :: t -> loop (h :: final_lst) t    
  in
    loop [] [dir]
    

let read_file_tree () = Sys.getcwd () |> read_dir

(* run file --mime and checks for "charset=binary"; *)
let is_binary fname = 
  let cmd = "file --mime " ^ fname in
  Utils.run_cmd cmd |> String.split_on_char '=' |>
    function 
      | _ :: h2 :: [] -> (String.trim h2) = "binary" 
      | _ -> Log.log Warning @@ "invalid file --mime result for file : " ^ fname; false


(* FIXME: calling is_binary many times can cause performance issues as it is running a shell command every time.
  To fix this, file --mime can be called once to many files, but would be necessary a better lookup algorithm to check all the output at once*)
let should_ignore (args: Types.command_args) fname  = 
  let exception Found in
  if is_binary fname then ( true ) else
  try
    args.ignore_files |> List.iter (fun istr -> 
        let re = Str.regexp_string istr in
        (* will raise Not_found if it doesn't find *)
        try
          (Str.search_forward re fname 0) |> ignore;
          (raise Found)
        with
          | Not_found -> ()
    );
    false 
  with
    | Found -> true    