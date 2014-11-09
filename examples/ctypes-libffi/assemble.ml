open Assemblage

let username =
  bin "username" [ unit "main"; pkg "ctypes"; pkg "ctypes.foreign" ]

let () = assemble (Project.v "username" [username])
