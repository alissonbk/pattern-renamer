open Printf


let renamer recursive ignore multiple_from multiple_to from_word to_word =
  let lst_to_string = List.fold_left (fun acc curr -> acc ^ "," ^ curr) "" in
  printf "recursive: %b\nignore: %s\nmultiple_from: %s\nmultiple_to: %s\nfrom_word: %s\nto_word: %s\n" 
    recursive (lst_to_string ignore) (lst_to_string multiple_from) (lst_to_string multiple_to) from_word to_word