open Assemblage

let a = unit "a" (`Dir ".")
let b = unit "b" (`Dir ".")

let b1 = bin "b1" [b; a]

let b2 = bin "b2" [b]

let l = lib "l" [a]

let b3 = bin "b3" [b] ~deps:[l] ~link_all:true

let () =
  add (create "containers" [b1;b2;b3])
