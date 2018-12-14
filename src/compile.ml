open Config
open Util

let get_liqpack_config dir_path config need_files =
  let path = abs_path dir_path ^ "/liqpack" in
  let liqpack_path = if Sys.file_exists path then path else raise (Error "no liqpack file found") in
  LiqpackConfig.parse_liqpack (LiqpackConfig.read_liqpack liqpack_path) config need_files

let compile dir_path ?(arg = "") config  =
  let liqpack_config = get_liqpack_config dir_path config true in
  let files_string = List.fold_left (fun acc x -> acc ^ " " ^ x) "" liqpack_config.files in
  let _ = Sys.chdir dir_path in
  let run_command main_opt = 
    let command = 
      "%s %s %s %s" #< 
        config.path 
        files_string 
        (match main_opt with | None -> "" | Some x -> "--main %s" #< x)
        (match liqpack_config.options with | Some x -> x | None -> "") ^ " " ^ arg
    in
    if Sys.command command = 0 then
      match (liqpack_config.output, main_opt) with
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
    | [] ->
      ()
  in
  loop liqpack_config.main (List.length liqpack_config.main = 0)
