open Sexplib

let (#<) = Printf.sprintf

type config = {
  path: string;
  libs: (string, string) Hashtbl.t;  
}

let init_config = "
(
  (path nil)
  (libs ())
)
"

let parse_config config_raw =
  let rec loop sexp result =
    match sexp with
    | Sexp.List (Sexp.Atom "path" :: Sexp.Atom value :: []) ->
      { result with path = value }
    | Sexp.List (Sexp.Atom key :: Sexp.Atom value :: []) ->
      Hashtbl.add result.libs key value;
      result
    | Sexp.List sexp_lst ->
      List.fold_left (fun acc x -> loop x acc) result sexp_lst
    | _ ->
      result
  in
  loop config_raw {path = ""; libs = Hashtbl.create 256;}


let write_config sexp =
  Sexp.save_hum "liqpack.cfg" sexp

let read_config () =
  try
    Sexp.load_sexp "liqpack.cfg"
  with Sys_error _ ->
    write_config (Sexp.of_string init_config);
    Sexp.load_sexp "liqpack.cfg"


let sys_arg_parse config =
  match Sys.argv with
  | [| _ ; command ; arg |] -> print_endline ("good command" ^ command ^ arg)
  | [| _ ; command |] -> print_endline ("bad command" ^ command)
  | _ -> print_endline "
  Current path: %s

  setpath <liquidity path>
  install <package git url>
  remove <package name>
  list
  " #< config.path

let () =
  let config_raw = read_config () in
  let config = parse_config config_raw in
  let _ = sys_arg_parse config in
  ()
