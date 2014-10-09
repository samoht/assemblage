open Assemblage

let dir = [ "src" ]

let a = unit "a" ~dir
let b =
  let dir = dir / "b" in
  pack "b" [ unit "a" ~dir; unit "c" ~dir; ]

let main = bin "main" ~deps:[a;b] [ unit "main" ~dir ]

let () = assemble (project "pack" [main])
