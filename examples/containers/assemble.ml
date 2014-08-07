open Assemblage

let a = unit "a" (`Dir ".")
let b = unit "b" (`Dir ".")

let b1 = bin "b1" (`Units [b; a])

let b2 = bin "b2" (`Units [b])

let l = lib "l" (`Units [a])

let b3 = bin "b3" (`Units [b]) ~deps:[l] ~link_all:true

let () =
  add (create "containers" [b1;b2;b3])
