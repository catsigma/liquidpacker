open Config
open Util

let get_liquidpacker_config dir_path config need_files =
  let path = abs_path dir_path ^ "/liquidpacker" in
  let liquidpacker_path = if Sys.file_exists path then path else raise (Error "no liquidpacker file found") in
  LiquidpackerConfig.parse_liquidpacker (LiquidpackerConfig.read_liquidpacker liquidpacker_path) config need_files

let compile dir_path ?(arg = "") config  =
  let liquidpacker_config = get_liquidpacker_config dir_path config true in
  let files_string = List.fold_left (fun acc x -> acc ^ " " ^ x) "" liquidpacker_config.files in
  let _ = Sys.chdir dir_path in
  let run_command main_opt = 
    let _ = print_newline () in
    let command = 
      "%s %s %s %s" #< 
        config.path 
        files_string 
        (match main_opt with | None -> "" | Some x -> "--main %s" #< x)
        (match liquidpacker_config.options with | Some x -> x | None -> "") ^ " " ^ arg
    in
    if Sys.command command = 0 then
      match (liquidpacker_config.output, main_opt) with
      | Some output_dir, Some main_name ->
        let main_file_name = 
          (String.mapi (fun i c -> if i = 0 then Char.lowercase_ascii c else c) main_name)
        in
        List.iter (fun surfix -> 
          let file_name = main_file_name ^ surfix in
          move_file file_name output_dir
        ) [".liq.tz"; ".liq.tz.json"; ".liq.init.tz"; ".liq.init.json"]

      | _ -> ()

    else
      raise (Error "Compilation failed!")
  in
  let rec loop main_lst is_initially_empty =
    match main_lst with
    | hd :: tl ->
      let _ = run_command (Some hd) in
      loop tl is_initially_empty
    | [] when is_initially_empty ->
      run_command None
    | [] -> ()
  in
  loop liquidpacker_config.main (List.length liquidpacker_config.main = 0)
