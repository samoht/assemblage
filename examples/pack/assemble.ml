open Assemblage

let a = unit "a" (`Path ["src"])

let b = pack "b" [
    unit "a" (`Path ["src";"b"]);
    unit "c" (`Path ["src";"b"]);
  ]

let main = bin "main" ~deps:[a;b] (`Units [
    unit "main" (`Path ["src"])
  ])

let () = assemble (project "pack" [main])
