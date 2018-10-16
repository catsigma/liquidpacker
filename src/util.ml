exception Error of string 

let git_clone name url =
  Sys.command ("git clone " ^ url ^ " libs/" ^ name)

let abs_path path = 
  let cwd = Sys.getcwd () in
  if Filename.is_relative path then
    cwd ^ "/" ^ path
  else
    path
    