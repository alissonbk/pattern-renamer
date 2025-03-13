(* open Cmdliner *)
open Printf


exception Invalid_parameter of string
type cmd_options_t =
  (* -r : Recursive -> enter in subdirectories*)
  | Recursive
  (* -i : Ignore -> should contain files or directories to be ignored, separated by comma if more than 1*)
  | Ignore of string list
  (* -m : Multiple -> rename multiple at the same time *)
  | Multiple of string list
  (* LastParam -> default and will be used when the option Multiple is not beeing used *)
  | LastParam of string


(* retrieve comma separated parameters from options *)
let rec read_option_parameters_r (lst : string list) (arg : string) (curr_idx : int) : string list =  
  match (String.trim arg) with    
    | s when String.contains s ',' -> 
      let v = s |> String.split_on_char ',' |> List.hd in
      read_option_parameters_r (v :: lst) Sys.argv.(curr_idx + 1) (curr_idx + 1)
    | s when s <> "" -> (s :: lst)
    | _ -> raise (Invalid_parameter "invalid option parameter \"\"")
let read_option_parameters = read_option_parameters_r []   

let retrieve_option_with_values arg curr_idx =    
  match (String.trim arg) with
    | "-r" -> Some Recursive
    | "-i" -> 
      let params = read_option_parameters Sys.argv.(curr_idx + 1) (curr_idx + 1) in
      Some (Ignore params)
    | "-m" -> 
      let params = read_option_parameters Sys.argv.(curr_idx + 1) (curr_idx + 1) in
      Some (Multiple params)
    | _ -> None

(* read the args *)
let entrypoint () =    
  printf "number of args: %d\n" (Array.length Sys.argv);
  let last_pos = Array.length Sys.argv - 1 in  
  let rec loop (lst : cmd_options_t list) (curr_idx : int) =        
    match curr_idx with    
    | n when n > last_pos -> lst
    | n when n = last_pos -> loop (LastParam (String.trim Sys.argv.(n)) :: lst) (curr_idx + 1)
    | _ ->            
      (match retrieve_option_with_values Sys.argv.(curr_idx) curr_idx with
        | None -> loop lst (curr_idx + 1)
        | Some t -> loop (t :: lst) (curr_idx + 1)
      )
  in
  loop [] 1


let () =
  entrypoint () |> List.iter (function 
    | Recursive -> printf "recursive"; printf "\n"
    | Ignore lst -> printf "Ignore: \n\t"; lst |> List.iter (printf "%s - "); printf "\n"
    | Multiple lst -> printf "Multiple: \n\t"; lst |> List.iter (printf "%s - "); printf "\n"
    | LastParam lp -> printf "LastParam: \t%s" lp; printf "\n"
  )