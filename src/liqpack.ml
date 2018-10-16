open Util

let (#<) = Printf.sprintf

let sys_arg_parse config =
  match Sys.argv with
  | [| _ ; "install" ; name; meth; url |] -> 
    let _ = Hashtbl.replace config.libs name (meth, url) in
    gen_config config |> write_config

  | [| _ ; "setpath" ; arg |] -> gen_config {config with path = arg} |> write_config
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
  let config_raw = read_config () in
  let config = parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
