open Assemblage

let a =
  lib "lib1" [cu "a" ~dir:"a" [pkg "ezjsonm"]]

let b =
  let b = cu "b" ~dir:"b" [a] in
  let c = cu "c" ~dir:"b" [b] in
  lib "lib2" [b; c]

let () =
  create "multi-libs" [a; b]
