open Cmdliner

let recursive =
  let doc = "Read sub directories recursively." in
  Arg.(value & flag & info ["r"; "recursive"] ~docv:"Recursive" ~doc)

let ignore_files =
  let doc = "List of files to be ignored." in
  Arg.(value & opt (list string) [] & info ["i"; "ignore"] ~docv:"Ignore" ~doc)

let ignore_patterns =
  let doc = "List of patterns to be ignored (normally something including the pattern that will actually be changed)." in
  Arg.(value & opt (list string) [] & info ["ip"; "ignore-pattern"] ~docv:"Ignore pattern" ~doc)

let multiple_from =
  let doc = "List of words to be replaced. The pos 0 (from) argument will be ignored" in
  Arg.(value & opt (list string) [] & info ["mf"; "multiple-from"] ~docv:"Multiple from" ~doc)

let multiple_to =
  let doc = "List of words to replace, 1 to 1 with multiple_from (must be sorted and have same order of multiple_from). The pos 1 (to) argument will be ignored" in
  Arg.(value & opt (list string) [] & info ["mt"; "multiple-to"] ~docv:"Multiple to" ~doc)

let from_word =
  let doc = "The word pattern which will be replaced \n - Can be written in any pattern as simpleExample or simple_exmaple ... - When multiple from (--mf) is used this will be ignored" in  
  Arg.(value & pos 0 string "" & info [] ~docv:"From" ~doc)

let to_word =
  let doc = "The word that will replace the from word - Can be written in any pattern as simpleExample or simple_exmaple ... - When multiple to (--mt) is used this will be ignored" in  
  Arg.(value & pos 1 string "" & info [] ~docv:"To" ~doc)

let debug_mode =
  let doc = "Show debugging logs" in
  Arg.(value & flag & info ["debug"] ~docv:"Debug mode" ~doc)

let entrypoint_term = Term.(const Core.entrypoint $ recursive $ ignore_files $ ignore_patterns $ multiple_from $ multiple_to $ from_word $ to_word $ debug_mode)

let info =
  let doc = "Rename multiple words by pattern ex: someExample" in
  let man = [
    `S Manpage.s_bugs;
    `P "." ]
  in
  Cmd.info "renamer" ~version:"%‌%VERSION%%" ~doc ~exits:Cmd.Exit.defaults ~man

let command_value = Cmd.v info entrypoint_term