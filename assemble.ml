open Assemblage

(* OCamlfind packages *)

let cmdliner = pkg "cmdliner"
let bytecomp = pkg "compiler-libs.bytecomp"
let toplevel = pkg "compiler-libs.toplevel"

(* Library *)

let ocaml_version =
  try
    let i = String.index Sys.ocaml_version '.' in
    let j = String.index_from Sys.ocaml_version (i+1) '.' in
    let major = String.sub Sys.ocaml_version 0 i in
    let minor = String.sub Sys.ocaml_version (i+1) (j-i-1) in
    int_of_string major, int_of_string minor
  with _ ->
    Printf.eprintf "Unknown OCaml version: %s\n%!" Sys.ocaml_version;
    exit 1

let lib =
  let as_OCaml_incl =
    unit "as_OCaml_incl" ~deps:[bytecomp]
      (if ocaml_version < (4,2) then (`Path ["lib";"401"])
       else (`Path ["lib";"402"]))
  in
  let unit ?deps name = unit ?deps name (`Path ["lib"]) in
  lib "assemblage" ~deps:[cmdliner]
    (`Units [
        unit "as_shell";
        unit "as_git";
        unit "as_makefile";
        unit "as_features";
        unit "as_flags";
        unit "as_resolver";
        unit "as_build_env";
        unit "as_action";
        unit "as_component";
        unit "as_project";
        unit "as_opam";
        unit "as_ocamlfind";
        unit "as_project_makefile";
        as_OCaml_incl;
        unit "as_OCaml" ~deps:[bytecomp];
        unit "as_env";
        unit "as_tool";
        unit "as_cmd";
        unit "assemblage";
      ])

let assemblage_tool =
  let us = `Units [ unit "tool" (`Path ["bin"]) ~deps:[toplevel] ] in
  bin "assemblage" ~deps:[lib] ~linkall:true ~native:false us

let ctypes_gen =
  let us = `Units [ unit "ctypes_gen" (`Path ["bin"]) ] in
  bin "ctypes-gen" ~deps:[lib] ~native:false us

let assemble_assemble =
  (* Sanity check, can we compile assemble.ml to native code ? *)
  let us = `Units [ unit "assemble" (`Path []) ] in
  bin "assemble" ~deps:[lib] ~install:false us

(* Tests & examples *)

let mk_test ?(example = false) name =
  let base = if example then "examples/" else "test/" in
  let dir = base ^ name in
  let args cmd r =
    [ cmd; "--auto-load=false"; "-I"; root_dir r / build_dir lib r; ]
  in
  test name ~dir [
    test_bin assemblage_tool ~args:(args "describe") ();
    test_bin assemblage_tool ~args:(args "setup") ();
    test_shell "make";
    test_shell "make distclean";
  ]

let mk_example = mk_test ~example:true

let tests = [
  mk_example "hello";
  mk_example "camlp4";
  mk_example "multi-libs";
  mk_example "containers";
  mk_example "pack";
]

(* Docs *)

let dev_doc = doc ~install:false "dev" [lib]
let doc = doc "public" [pick "assemblage" lib]

(* The project *)

let p = project "assemblage"
    ([ lib;
       assemblage_tool; ctypes_gen; assemble_assemble;
       dev_doc; doc ] @ tests)

let () = assemble p
