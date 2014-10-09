open Assemblage

let hello = bin "hello" [ unit "main" ]
let () = assemble (project "hello" [hello])
