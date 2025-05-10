open Types



let log ?(flush_t=None) (lvl: log_level) message =
  let reset_ppf = Spectrum.prepare_ppf Format.std_formatter in  
  match lvl with
    | Success -> Format.printf "@{<green>%s@}\n" @@ "[Success] " ^ message
    | Ask -> Format.printf "%s\n" @@ message
    | Info -> Format.printf "@{<blue>%s@}\n" @@ "[Info] " ^ message
    | Warning -> Format.printf "@{<orange>%s@}\n\n" @@ "[Warning] " ^ message 
    | Error -> Format.printf "@{<red>%s@}\n\n" @@ "[Error] " ^ message;
    | Debug -> Format.printf "@{<#964B00>%s@}\n\n" @@ "[Debug] " ^ message;
  
  match flush_t with
    | None -> ()
    | All -> flush_all ();
    | Stdout -> flush stdout;

  reset_ppf ()

let log_nd_fail message =  
  log Error message ~flush_t:All;
  failwith message


let log_flow_type = function        
        | Types.Multiple -> log Info "flow_type: Multiple"
        | Types.MultipleFromSingleTo -> log Info "flow_type: MultipleFromSingleTo"


let log_patterns (all_patterns: Types.all_patterns) =        
    let f p = p |> List.map (fun lst -> List.map (fun p -> 
        match p with
            | Types.Underscore v -> "Underscore: " ^ (Utils.unbox_extp v)
            | Types.CamelCase v -> "CamelCase: " ^ v
            | Types.CapitalizedCamelCase v -> "CapitalizedCamelCase: " ^ v
            | Types.SpaceSeparated v -> "SpaceSeparated: " ^ (Utils.unbox_extp v)
            | Types.Lower v -> "Lower: " ^ v
            | Types.InvalidPattern -> "invalid pattern"
        ) 
        lst) 
        |> Utils.mtx_to_string |> log Debug
    in
    f all_patterns.from_lst;
    f all_patterns.to_lst

let log_input_args (args : Types.command_args) =   
    log Debug @@ Printf.sprintf "recursive: %b\nignore_files: %s\nignore_patterns: %s\nfrom_words: %s\nto_words: %s\n" 
    args.recursive (Utils.lst_to_string args.ignore_files) (Utils.lst_to_string args.ignore_patterns) (Utils.lst_to_string args.from_words) (Utils.lst_to_string args.to_words)