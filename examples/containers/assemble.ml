open Assemblage

let a = unit "a" (`Path [])
let b = unit "b" (`Path [])

let b1 = bin "b1" (`Units [b; a])

let b2 = bin "b2" (`Units [b])

let l = lib "l" (`Units [a])

let b3 = bin "b3" (`Units [b]) ~deps:[l] ~linkall:true

let () = assemble (project "containers" [b1;b2;b3])
