open Assemblage

(* OCamlfind packages *)

let cmdliner = pkg    "cmdliner"
let graph    = pkg    "ocamlgraph"
let compiler = pkg    "compiler-libs.toplevel"
let optcomp  = pkg_pp "optcomp"

(* Library *)

let lib =
  lib (ocamldep ~dir:"lib" [cmdliner; graph; compiler; optcomp]) "assemblage"

let configure =
  let configure = cu ~dir:"bin" [lib] "configure" in
  bin ~link_all:true ~byte_only:true [configure] "configure.ml"

let describe =
  let describe = cu ~dir:"bin" [lib] "describe" in
  bin ~link_all:true ~byte_only:true [describe] "describe.ml"

let ctypes_gen =
  let ctypes_gen = cu ~dir:"bin" [lib] "ctypes_gen" in
  bin ~byte_only:true [ctypes_gen] "ctypes-gen"

(* Tests *)

let mk_test name =
  let dir = "examples/" ^ name in
  let args build_dir = [
    "--disable-auto-load"; "-I"; build_dir lib;
  ] in
  test ~dir [] [
    test_bin describe args;
    test_bin configure args;
    test_shell "make";
    test_shell "make distclean";
  ] name

let camlp4     = mk_test "camlp4"
let multi_libs = mk_test "multi-libs"

(* The project *)

let () =
  create
    [lib; configure; describe; ctypes_gen; camlp4; multi_libs]
    "assemblage"
