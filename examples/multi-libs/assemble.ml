open Assemblage

let a =
  lib "lib-a" [cu "a" ~dir:"a" [pkg "ezjsonm"]]

let a1 =
  lib "lib-a-1" [cu "a1" ~dir:"a" [a]]

let b =
  let b = cu "b" ~dir:"b" [a] in
  let c = cu "c" ~dir:"b" [b] in
  lib "lib2" [b; c]

let bin =
  bin "a-test" ~link_all:true ~deps:(fun _ -> [a; a1; b]) [cu "foo" []]

let () =
  create "multi-libs" [a; b; bin]
