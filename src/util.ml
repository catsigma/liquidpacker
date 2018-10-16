open Sexplib

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

let check_n_create dir =
  let result = 
    try
      Sys.is_directory dir
    with Sys_error _ ->
      false
  in
  if not result then
    Unix.mkdir dir 0o777
  else
    ()

let config_path = 
  let dir = Unix.getenv "HOME" ^ "/.liqpack" in
  let libs_dir = Unix.getenv "HOME" ^ "/.liqpack/libs" in
  check_n_create dir;
  check_n_create libs_dir;
  dir ^ "/config"

let write_config sexp =
  Sexp.save_hum config_path sexp

let read_config () =
  try
    Sexp.load_sexp config_path
  with Sys_error _ ->
    write_config init_config;
    Sexp.load_sexp config_path

let git_clone name url =
  Sys.command ("git clone " ^ url ^ " libs/" ^ name)
