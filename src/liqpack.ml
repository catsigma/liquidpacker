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
      | [| _ ; "list" |] ->
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

      | [| _ ; "install" ; raw_path |] ->
        let rec install path =
          let liqpack_config, from =
            if is_git_url path then
              let (temp_dir, result) = BaseConfig.install_from_git path in
              if result then
                Compile.get_liqpack_config temp_dir config false, "git"
              else
                let _ = Sys.command ("rm -rf %s" #< temp_dir) in
                raise (Error "git clone failed")
            else
              let path = if path = raw_path then path else raw_path ^ "/" ^ path in
              Compile.get_liqpack_config path config false, "local"
          in
          if liqpack_config.name = "" then
            raise (Error "no name field in liqpack file")
          else
            let _ = 
              if from = "git" then
                let _ = BaseConfig.move_for_git liqpack_config.name in
                Hashtbl.replace config.libs liqpack_config.name ("git", path)
              else
                let path = if path = raw_path then path else raw_path ^ "/" ^ path in
                Hashtbl.replace config.libs liqpack_config.name ("local", abs_path path)
            in
            List.iter (fun (_,x) -> install x) liqpack_config.deps
        in
        install raw_path;
        BaseConfig.gen_config config |> BaseConfig.write_config

      | _ -> print_endline "
  Liquidity path: %s

  setpath <liquidity path>
  build <target dir path> <?liquidity arguments>      
  install <name> <dir path/git url>
  remove <package name>
  list
      " #< config.path

let () =
  let config_raw = BaseConfig.read_config () in
  let config = BaseConfig.parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
