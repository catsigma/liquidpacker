open Util
open Config

let sys_arg_parse (config : config) =
  match Sys.argv with
  | [| _ ; "setpath" ; arg |] -> BaseConfig.gen_config {config with path = arg} |> BaseConfig.write_config
  | argv -> 
    if config.path = "nil" || config.path = "" then
      print_endline "Please run `liqpack setpath <location of liquidity executable file>`"
    else
      match argv with
      | [| _ ; "check" |] ->
        let path = config.path in
        let libs = Hashtbl.fold 
          (fun k (m, _) acc -> "%s\n    %s (%s)" #< acc k m) 
          config.libs ""
        in
        print_endline ("
  Liquidity path:
    %s

  Libraries:%s
        " #< path libs)

      | [| _ ; "remove" ; pack_name; |] ->
        BaseConfig.remove_package pack_name config

      | [| _ ; "build" ; dir_path; |] ->
        Compile.compile dir_path config

      | [| _ ; "build" ; dir_path; arg |] ->
        Compile.compile dir_path ~arg config 

      | [| _ ; "install" ; name; path |] ->
        (* TODO: remove name *)
        if is_git_url path then
          let _ = Hashtbl.replace config.libs name ("git", path) in
          if BaseConfig.install_from_git name path = 0 then
            BaseConfig.gen_config config |> BaseConfig.write_config
          else
            raise (Error "git clone failed")
        else
          let abs_dir = abs_path path in
          let _ = Hashtbl.replace config.libs name ("local", abs_dir) in
          BaseConfig.gen_config config |> BaseConfig.write_config

      | _ -> print_endline "
  Liquidity path: %s

  setpath <liquidity path>
  build <target dir path> <?liquidity arguments>      
  install <name> <dir path/git url>
  remove <package name>
  check
      " #< config.path

let () =
  let config_raw = BaseConfig.read_config () in
  let config = BaseConfig.parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
