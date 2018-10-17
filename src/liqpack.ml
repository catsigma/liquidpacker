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
      | [| _ ; "build" ; dir_path; |] ->
        Compile.compile dir_path config

      | [| _ ; "build" ; dir_path; arg |] ->
        Compile.compile dir_path ~arg config 

      | [| _ ; "install" ; name; "local"; dir |] -> 
        let abs_dir = abs_path dir in
        let _ = Hashtbl.replace config.libs name ("local", abs_dir) in
        BaseConfig.gen_config config |> BaseConfig.write_config

      | [| _ ; "install" ; name; "git"; url |] -> 
        let _ = Hashtbl.replace config.libs name ("git", url) in
        BaseConfig.gen_config config |> BaseConfig.write_config

      | [| _ ; command ; arg |] -> print_endline ("good command" ^ command ^ arg)
      | [| _ ; command |] -> print_endline ("bad command" ^ command)
      | _ -> print_endline "
  Liquidity path: %s

  setpath <liquidity path>
  build <target dir path> <?liquidity arguments>      
  install <name> <local/git> <location>
  remove <package name>
  check
      " #< config.path

let () =
  let config_raw = BaseConfig.read_config () in
  let config = BaseConfig.parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
