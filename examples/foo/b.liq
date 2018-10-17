contract Y = struct
  type storage = unit
  let%entry bar (parameter : unit) storage =
    ([] : operation list), storage
end