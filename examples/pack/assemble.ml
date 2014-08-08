open Assemblage

let a = unit "a" (`Dir "src")

let b = pack "b" [
    unit "a" (`Dir "src/b");
    unit "c" (`Dir "src/b");
  ]

let main = bin "main" ~deps:[a;b] (`Units [
    unit "main" (`Dir "src")
  ])

let () =
  add (create "pack" [main])
