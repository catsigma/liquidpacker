contract X = struct
  type storage = unit
  let%entry foo (parameter : unit) storage =
    ([] : operation list), storage
end