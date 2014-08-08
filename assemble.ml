open Assemblage

(* OCamlfind packages *)

let cmdliner = pkg    "cmdliner"
let graph    = pkg    "ocamlgraph"
let compiler = pkg    "compiler-libs.toplevel"
let optcomp  = pkg_pp "optcomp"

(* Library *)

let lib =
  let unit ?deps name = unit ?deps name (`Dir "lib") in
  lib "assemblage"
    ~deps:[cmdliner; graph]
    (`Units [
        unit "as_features";
        unit "as_flags";
        unit "as_resolver";
        unit "as_shell";
        unit "as_git";
        unit "as_build_env";
        unit "as_action";
        unit "as_project";
        unit "as_opam";
        unit "as_ocamlfind";
        unit "as_makefile";
        unit "as_OCaml" ~deps:[optcomp; compiler];
        unit "assemblage" ~deps:[compiler];
      ])

let configure =
  bin "configure.ml" ~deps:[lib] ~link_all:true ~native:false (`Units [
      unit "configure" (`Dir "bin")
    ])

let describe =
  bin "describe.ml" ~deps:[lib] ~link_all:true ~native:false (`Units [
      unit "describe" (`Dir "bin")
    ])

let ctypes_gen =
  bin "ctypes-gen" ~deps:[lib] ~native:false (`Units [
      unit "ctypes_gen" (`Dir "bin")
    ])

let assemblage_tool =
  let us = `Units [ unit "tool" (`Dir "bin") ] in
  bin "assemblage" ~deps:[lib] ~link_all:true ~native:false us

(* Tests *)

let mk_test name =
  let dir = "examples/" ^ name in
  let args r = [
    "--disable-auto-load"; "-I"; root_dir r / build_dir lib r;
  ] in
  test name ~dir [
    test_bin describe ~args ();
    test_bin configure ~args ();
    test_shell "make";
    test_shell "make distclean";
  ]

let tests = [
  mk_test "camlp4";
  mk_test "multi-libs";
  mk_test "containers";
  mk_test "pack";
]

(* Docs *)

let dev_doc = doc ~install:false "dev" [lib]
let doc = doc "public" [pick "assemblage" lib]

(* The project *)

let p =
  let cs = [lib; configure; describe; ctypes_gen; assemblage_tool;
            dev_doc; doc ] @ tests
  in
  create "assemblage" cs

let () = add p; assemblage p
