

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
