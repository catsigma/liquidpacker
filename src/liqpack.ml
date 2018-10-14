open Sexplib

let (#<) = Printf.sprintf
exception Error of string

type config = {
  path: string;
  libs: (string, string * string) Hashtbl.t;  
}

let construct_config path libs = 
  Sexp.List [
    Sexp.List [Sexp.Atom "path"; Sexp.Atom path;];
    Sexp.List [Sexp.Atom "libs"; Sexp.List libs;];
  ]

let init_config = 
  construct_config "nil" []

let parse_config config_raw =
  let rec loop sexp result =
    match sexp with
    | Sexp.List (Sexp.Atom "path" :: Sexp.Atom value :: []) ->
      { result with path = value }
    | Sexp.List (Sexp.Atom name :: Sexp.Atom meth :: Sexp.Atom location :: []) ->
      Hashtbl.add result.libs name (meth, location);
      result
    | Sexp.List sexp_lst ->
      List.fold_left (fun acc x -> loop x acc) result sexp_lst
    | _ ->
      result
  in
  loop config_raw {path = ""; libs = Hashtbl.create 256;}

let gen_config config =
  construct_config config.path 
    (Hashtbl.fold 
      (fun k (m, u) acc -> Sexp.List [Sexp.Atom k; Sexp.Atom m; Sexp.Atom u] :: acc) 
      config.libs [])

let write_config sexp =
  Sexp.save_hum "liqpack.cfg" sexp

let read_config () =
  try
    Sexp.load_sexp "liqpack.cfg"
  with Sys_error _ ->
    write_config init_config;
    Sexp.load_sexp "liqpack.cfg"

let git_clone name url =
  Sys.command ("git clone " ^ url ^ " libs/" ^ name)

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
