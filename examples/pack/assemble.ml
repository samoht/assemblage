open Assemblage

let dir = root / "src"

let a = unit "a" ~dir
let b =
  let dir = dir / "b" in
  (* FIXME *)
  pack "b" [ unit "a" ~dir; unit "c" ~dir; ]

let main = bin "main" [ unit "main" ~dir; a; b ]

let () = assemble (Project.v "pack" [main])
