open Util
open Config

let (#<) = Printf.sprintf

let sys_arg_parse (config : BaseConfig.t) =
  match Sys.argv with
  | [| _ ; "install" ; name; "local"; dir |] -> 
    let abs_dir = abs_path dir in
    let _ = Hashtbl.replace config.libs name ("local", abs_dir) in
    BaseConfig.gen_config config |> BaseConfig.write_config

  | [| _ ; "install" ; name; "git"; url |] -> 
    let _ = Hashtbl.replace config.libs name ("git", url) in
    BaseConfig.gen_config config |> BaseConfig.write_config

  | [| _ ; "setpath" ; arg |] -> BaseConfig.gen_config {config with path = arg} |> BaseConfig.write_config
  | [| _ ; command ; arg |] -> print_endline ("good command" ^ command ^ arg)
  | [| _ ; command |] -> print_endline ("bad command" ^ command)
  | _ -> print_endline "
  Current path: %s

  setpath <liquidity path>
  install <name> <method> <location>
  remove <package name>
  list
  " #< config.path

let () =
  let config_raw = BaseConfig.read_config () in
  let config = BaseConfig.parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
