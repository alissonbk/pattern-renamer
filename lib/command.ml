open Cmdliner

let recursive =
  let doc = "Read sub directories recursively." in
  Arg.(value & flag & info ["r"; "recursive"] ~docv:"Recursive" ~doc)

let ignore_files =
  let doc = "List of files to be ignored." in
  Arg.(value & opt (list string) [] & info ["i"; "ignore"] ~docv:"Ignore" ~doc)

let ignore_patterns =
  let doc = "List of patterns to be ignored (use a \\\"\\$\\\"\ where the pattern will be example: json:\"\\\"\\$\\\"\")." in
  Arg.(value & opt (list string) [] & info ["ip"; "ignore-pattern"] ~docv:"Ignore pattern" ~doc)

let yes =
    let doc = "Automatically accept all file changes" in  
    Arg.(value & flag & info ["y"; "yes"] ~docv:"Yes" ~doc)

let from_words =
  let doc = "The word(s) pattern(s) which will be replaced \n - Can be written in any pattern as simpleExample or simple_example ..." in  
  Arg.(value & pos 0 (list string) [] & info [] ~docv:"From word(s)" ~doc)

let to_words =
  let doc = "The word that will replace the from word - Can be written in any pattern as simpleExample or simple_example ..." in  
  Arg.(value & pos 1 (list string) [] & info [] ~docv:"To word(s)" ~doc)



let debug_mode =
  let doc = "Show debugging logs" in
  Arg.(value & flag & info ["debug"] ~docv:"Debug mode" ~doc)

let entrypoint_term = Term.(const Core.entrypoint $ recursive $ ignore_files $ ignore_patterns $ yes $ from_words $ to_words $ debug_mode)

let info =
  let doc = "Rename multiple words by pattern ex: someExample" in
  let man = [
    `S Manpage.s_bugs;
    `P "." ]
  in
  Cmd.info "pattern-renamer" ~version:"v0.0.4" ~doc ~exits:Cmd.Exit.defaults ~man

let command_value = Cmd.v info entrypoint_term