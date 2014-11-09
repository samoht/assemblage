open Assemblage

let hello = bin "hello" [ unit "main" ]
let () = assemble (Project.v "hello" [hello])
