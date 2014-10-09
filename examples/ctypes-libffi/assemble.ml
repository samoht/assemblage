open Assemblage

let username =
  let deps = [pkg "ctypes"; pkg "ctypes.foreign" ] in
  bin "username" ~deps [ unit "main" ]

let () = assemble (project "username" [username])
