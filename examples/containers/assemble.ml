open Assemblage

let a = unit "a"
let b = unit "b"

let b1 = bin "b1" [b; a]
let b2 = bin "b2" [b]
let l = lib "l" [a]
let b3 = bin "b3" [b; l] ~args:Args.linkall

let () = assemble (Project.v "containers" [b1; b2; b3])
