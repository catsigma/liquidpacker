open Sexplib
open Util

type config = {
  path: string;
  libs: (string, string * string) Hashtbl.t;  
}

module LiqpackConfig = struct
  type t = {
    name: string;
    files: string list;
    main: string option;
    deps: (string * string) list;
  }

  let read_liqpack path =
    try
      Sexp.load_sexp path
    with Sys_error _ ->
      raise (Error "invalid liqpack file")

  let read_package (config : config) name =
    match Hashtbl.find_opt config.libs name with
    | Some ("local", dir) ->
      let dir = dir ^ "/" in
      dir, read_liqpack (dir ^ "liqpack")

    | Some ("git", _) ->
      let dir = Unix.getenv "HOME" ^ "/.liqpack/libs/%s/" #< name in
      dir, read_liqpack (dir ^ "liqpack")

    | _ ->
      raise (Error "package %s is not found" #< name)

  let rec parse_liqpack liqpack_raw (config : config) need_files =
    let rec loop sexp result =
      match sexp with
      | Sexp.List (Sexp.Atom "name" :: Sexp.Atom name :: []) ->
        { result with name }

      | Sexp.List (Sexp.Atom "main" :: []) ->
        result

      | Sexp.List (Sexp.Atom "main" :: Sexp.Atom main :: []) ->
        { result with main = Some main }

      | Sexp.List (Sexp.Atom "files" :: paths) ->
        let path_strings = List.map (fun x -> 
            match x with 
            | Sexp.List _ -> raise (Error "invalid `files` value in liqpack file")
            | Sexp.Atom x -> x
          ) paths
        in
        { result with files = result.files @ path_strings }

      | Sexp.List (Sexp.Atom "deps" :: deps) ->
        let deps = List.map (fun x -> 
            match x with 
            | Sexp.List (Sexp.Atom name :: Sexp.Atom info :: []) ->
              name, info
            | _ -> raise (Error "invalid `deps` value in liqpack file")
          ) deps
        in
        let deps_files = 
          if need_files then
            List.fold_left 
              (fun acc (x, _) -> 
                let dir, sexp = read_package config x in
                let combined = List.map (fun x -> dir ^ x) (parse_liqpack sexp config true).files in
                acc @ combined) [] deps
          else
            []
        in
        { result with files = deps_files @ result.files; deps }

      | Sexp.List sexp_lst ->
        List.fold_left (fun acc x -> loop x acc) result sexp_lst

      | _ ->
        result
    in
    loop liqpack_raw {
      name = "";
      files = [];
      main = None;
      deps = [];
    }
end


module BaseConfig = struct
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

  let remove_package name config = 
    match Hashtbl.find_opt config.libs name with
    | None ->
      raise (Error "package %s does not exist" #< name)

    | Some ("git", _) ->
      let _ = Hashtbl.remove config.libs name in
      let lib_dir = Unix.getenv "HOME" ^ "/.liqpack/libs/" ^ name in
      let _ = Sys.command "rm -rf %s" #< lib_dir in
      gen_config config |> write_config

    | _ ->
      let _ = Hashtbl.remove config.libs name in
      gen_config config |> write_config

  let install_from_git url =
    let temp_dir = Unix.getenv "HOME" ^ "/.liqpack/libs/_" in
    let _ = Sys.command ("rm -rf %s" #< temp_dir) in
    temp_dir, Sys.command ("git clone %s %s" #< url temp_dir) = 0

  let move_for_git name =
    let dir x = Unix.getenv "HOME" ^ "/.liqpack/libs/" ^ x in
    Sys.command ("mv %s %s" #< (dir "_") (dir name))

end

