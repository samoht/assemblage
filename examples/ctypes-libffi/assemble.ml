open Assemblage

let username =
  let deps = [pkg "ctypes"; pkg "ctypes.foreign" ] in
  bin "username" ~deps (`Units [ unit "main" (`Path []) ])

let () = assemble (project "username" [username])
