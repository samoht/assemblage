open Assemblage

let s = unit "s" (`Dir "a")
let a = lib "lib-a" [
    s;
    unit "a" (`Dir "a") ~deps:[s; pkg "ezjsonm"]]
let a1 = lib "lib-a-1" [unit "a1" (`Dir "a") ~deps:[a]]
let b =
  let b = unit "b" (`Dir "b") ~deps:[a] in
  let c = unit "c" (`Dir "b") ~deps:[b] in
  lib "lib2" [b; c]

let bin =
  bin "a-test" [unit "foo" (`Dir ".") ~deps:[a; a1; b]] ~link_all:true

let () = add (create "multi-libs" [a; b; bin])
