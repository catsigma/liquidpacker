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
  let len_s = String.length s in
  let len_p = String.length p in
  if len_s >= len_p then
    p = String.sub s 0 len_p
  else
    false
    
let is_git_url path =
  start_with path "git@" || 
  start_with path "http://" || 
  start_with path "https://" 


module Version = struct
  type t = int list

  let parse (s : string) =
    let lst = String.split_on_char '.' s in
    List.map int_of_string lst

  let stringify (v : t) =
    List.fold_left (fun acc v -> 
      if acc = "" then 
        "%d" #< v
      else
        "%s.%d" #< acc v
    ) "" v

  let compare (v1 : t) (v2 : t) =
    let rec cp lst1 lst2 =
      match (lst1, lst2) with
      | ([], []) ->
        0
      | (_ :: _, []) ->
        1
      | ([], _ :: _) ->
        -1
      | (num1 :: tl1, num2 :: tl2) ->
        if num1 = num2 then
          cp tl1 tl2
        else
          if num1 > num2 then
            1
          else
            -1

    in
    cp v1 v2

end