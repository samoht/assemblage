open Assemblage

let s = unit "s" ~dir:["a"]
let a = lib "lib-a" [s; unit "a" ~dir:["a"] ~deps:[s; pkg "ezjsonm"]]
let a1 = lib "lib-a-1" [unit "a1" ~dir:["a"] ~deps:[a]]
let b =
  let b = unit "b" ~dir:["b"] ~deps:[a] in
  let c = unit "c" ~dir:["b"] ~deps:[b] in
  lib "lib2" [b; c]

let bin =
  bin "a-test" [unit "foo" ~deps:[a; a1; b]] ~flags:Flags.linkall

let () = assemble (project "multi-libs" [a; b; bin])
