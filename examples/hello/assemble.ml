open Assemblage

let hello = bin "hello" (`Units [ unit "main" (`Path []) ])
let () = assemble (project "hello" [hello])
