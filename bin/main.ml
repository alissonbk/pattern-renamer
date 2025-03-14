open Cmdliner
open Printf


let renamer recursive ignore multiple from_word to_word =
  let lst_to_string = List.fold_left (fun acc curr -> acc ^ "," ^ curr) "" in
  printf "recursive: %b\nignore: %s\nmultiple: %s\nfrom_word: %s\nto_word: %s\n" 
    recursive (lst_to_string ignore) (lst_to_string multiple) from_word to_word

let recursive =
  let doc = "Read sub directories recursively." in
  Arg.(value & flag & info ["r"; "recursive"] ~docv:"Recursive" ~doc)

let ignore =
  let doc = "List of files to be ignored." in
  Arg.(value & opt (list string) [] & info ["i"; "ignore"] ~docv:"Ignore" ~doc)

let multiple =
  let doc = "List of words when replacing multiple words at the same time (will run in parallel)" in
  Arg.(value & opt (list string) [] & info ["m"; "multiple"] ~docv:"Multiple" ~doc)

let from_word =
  let doc = "The word pattern which will be replaced (can be written in any pattern as simpleExample or simple_exmaple ...)" in  
  Arg.(value & pos 0 string "" & info [] ~docv:"from" ~doc)

let to_word =
  let doc = "The word that will replace the from word (can be written in any pattern as simpleExample or simple_exmaple ...)" in  
  Arg.(value & pos 1 string "" & info [] ~docv:"to" ~doc)

let renamer_term = Term.(const renamer $ recursive $ ignore $ multiple $ from_word $ to_word)


let info =
  let doc = "Rename multiple words by pattern ex: someExample" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <hehey at example.org>." ]
  in
  Cmd.info "renamer" ~version:"%‌%VERSION%%" ~doc ~exits:Cmd.Exit.defaults ~man

let () = Stdlib.exit @@ Cmd.eval @@ Cmd.v info renamer_term