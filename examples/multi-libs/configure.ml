open Assemblage

let a =
  lib [cu ~dir:"a" [pkg "ezjsonm"] "a"] "lib1"

let b =
  let b = cu ~dir:"b" [a] "b" in
  let c = cu ~dir:"b" [b] "c" in
  lib [b; c] "lib2"

let () =
  create [a; b] "multi-libs"
