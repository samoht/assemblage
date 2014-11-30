open Assemblage

let dir_a = root / "a"
let s = unit "s" ~dir:dir_a
let a = lib "lib-a" [s; unit "a" ~dir:dir_a ~needs:[pkg "ezjsonm"]]
let a1 = lib "lib-a-1" [unit "a1" ~dir:dir_a ~needs:[a]]

let b =
  let dir = root / "b" in
  let b = unit "b" ~dir ~needs:[a] in
  let c = unit "c" ~dir in
  lib "lib2" [b; c]

let bin =
  bin "a-test" [unit "foo" ~needs:[a; a1; b]] ~args:Args.linkall

let () = assemble (Project.v "multi-libs" [a; b; bin])
