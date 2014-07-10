open Assemblage

(* OCamlfind packages *)

let cmdliner = pkg    "cmdliner"
let graph    = pkg    "ocamlgraph"
let compiler = pkg    "compiler-libs.toplevel"
let optcomp  = pkg_pp "optcomp"

(* Library *)

let lib =
  lib "assemblage"
    (ocamldep ~dir:"lib" [cmdliner; graph; compiler; optcomp])

let configure =
  let configure = cu "configure" ~dir:"bin" [lib] in
  bin "configure.ml" ~link_all:true ~byte_only:true [configure]

let describe =
  let describe = cu "describe" ~dir:"bin" [lib]  in
  bin "describe.ml" ~link_all:true ~byte_only:true [describe]

let ctypes_gen =
  let ctypes_gen = cu "ctypes_gen" ~dir:"bin" [lib] in
  bin "ctypes-gen" ~byte_only:true [ctypes_gen]

(* Tests *)

let mk_test name =
  let dir = "examples/" ^ name in
  let args build_dir = [
    "--disable-auto-load"; "-I"; build_dir lib;
  ] in
  test name ~dir [] [
    test_bin describe ~args ();
    test_bin configure ~args ();
    test_shell "make";
    test_shell "make distclean";
  ]

let camlp4     = mk_test "camlp4"
let multi_libs = mk_test "multi-libs"

(* The project *)

let () =
  create "assemblage"
    [lib; configure; describe; ctypes_gen; camlp4; multi_libs]
