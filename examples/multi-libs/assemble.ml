open Assemblage

let a = lib "lib-a" [unit "a" ~dir:"a" ~deps:[pkg "ezjsonm"]]
let a1 = lib "lib-a-1" [unit "a1" ~dir:"a" ~deps:[a]]
let b =
  let b = unit "b" ~dir:"b" ~deps:[a] in
  let c = unit "c" ~dir:"b" ~deps:[b] in
  lib "lib2" [b; c]

let bin =
  bin "a-test" [unit "foo" ~deps:[a; a1; b; ]] ~link_all:true

let () = add (create "multi-libs" [a; b; bin])
