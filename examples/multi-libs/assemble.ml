open Assemblage

let a =
  lib "lib-a" [unit "a" ~dir:"a" [pkg "ezjsonm"]]

let a1 =
  lib "lib-a-1" [unit "a1" ~dir:"a" [a]]

let b =
  let b = unit "b" ~dir:"b" [a] in
  let c = unit "c" ~dir:"b" [b] in
  lib "lib2" [b; c]

let bin =
  bin "a-test" ~link_all:true ~deps:(fun _ -> [a; a1; b]) [unit "foo" []]

let () =
  create "multi-libs" [a; b; bin]
