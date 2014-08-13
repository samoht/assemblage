open Assemblage

let s = unit "s" (`Path ["a"])
let a = lib "lib-a" (`Units [
    s;
    unit "a" (`Path ["a"]) ~deps:[s; pkg "ezjsonm"]
  ])
let a1 = lib "lib-a-1" (`Units [unit "a1" (`Path ["a"]) ~deps:[a]])
let b =
  let b = unit "b" (`Path ["b"]) ~deps:[a] in
  let c = unit "c" (`Path ["b"]) ~deps:[b] in
  lib "lib2" (`Units [b; c])

let bin =
  bin "a-test" (`Units [unit "foo" (`Path []) ~deps:[a; a1; b]]) ~link_all:true

let () = assemble (project "multi-libs" [a; b; bin])
