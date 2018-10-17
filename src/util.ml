exception Error of string 

let (#<) = Printf.sprintf

let abs_path path = 
  let cwd = Sys.getcwd () in
  if Filename.is_relative path then
    cwd ^ "/" ^ path
  else
    path
    
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

let start_with s p =
  p = String.sub s 0 (String.length p)

let is_git_url path =
  start_with path "git@" || 
  start_with path "http://" || 
  start_with path "https://" 
