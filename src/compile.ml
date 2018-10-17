open Config
open Util

let compile dir_path ?(arg = "") config  =
  let path = abs_path dir_path ^ "/liqpack" in
  let liqpack_path = if Sys.file_exists path then path else raise (Error "no liqpack file found") in
  let liqpack_config = LiqpackConfig.parse_liqpack (LiqpackConfig.read_liqpack liqpack_path) config in
  let files_string = List.fold_left (fun acc x -> acc ^ " " ^ x) "" liqpack_config.files in
  let _ = Sys.chdir dir_path in
  let command = 
    "%s %s %s %s" #< 
      config.path 
      files_string 
      (match liqpack_config.main with | None -> "" | Some x -> "--main %s" #< x) 
      arg
  in
  if Sys.command command = 0 then
    ()
  else
    raise (Error "Compilation failed!")
