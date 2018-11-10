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
  type predicate = LE | GE | EQ
  type t = {
    num: int list;
    predicate: predicate option;
  }

  let parse (s : string) (p : string) =
    let lst = String.split_on_char '.' s in
    let predicate = 
      match p with
      | "" -> None
      | ">=" -> Some GE
      | "<=" -> Some LE
      | "=" -> Some EQ
      | _ -> raise (Error "version predicate can only be `>=`, `<=` or `=`")
    in
    { num = List.map int_of_string lst;
      predicate; }

  let stringify (v : t) =
    let predicate = 
      match v.predicate with
      | None -> ""
      | Some p ->
        match p with | LE -> "<= " | GE -> ">= " | EQ -> "= "
    in
    predicate ^ List.fold_left (fun acc v -> 
      if acc = "" then 
        "%d" #< v
      else
        "%s.%d" #< acc v
    ) "" v.num

  let compare (v1 : t) (v2 : t) =
    let rec cp lst1 lst2 =
      match (lst1, lst2) with
      | ([], []) ->
        EQ
      | (_ :: _, []) ->
        GE
      | ([], _ :: _) ->
        LE
      | (num1 :: tl1, num2 :: tl2) ->
        if num1 = num2 then
          cp tl1 tl2
        else
          if num1 > num2 then
            GE
          else
            LE
    in
    cp v1.num v2.num

  let check (v : t) (r : t) =
    let compare_result = compare v r in
    match r.predicate, compare_result with
    | Some _, EQ -> true
    | None, _ -> true
    | Some GE, GE -> true
    | Some LE, LE -> true
    | _ -> false

end