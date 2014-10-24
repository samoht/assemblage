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

let lib_assemblage =
  let kind = `OCaml (`Both, `Hidden) in
  let unit ?deps ?(kind = kind) name = unit ?deps ~kind name ~dir:["lib"] in
  lib "assemblage"
    [ unit "as_string";
      unit "as_path";
      unit "as_cond";
      unit "as_context";
      unit "as_args";
      unit "as_env";
      unit "as_product";
      unit "as_rule";
      unit "as_part";
      unit "as_git";
      unit "as_project";
      unit "as_describe";
      unit "assemblage" ~kind:(`OCaml (`Both, `Normal)); ]

let lib_driver_make =
  let kind = `OCaml (`Both, `Hidden) in
  let as_ocaml_incl =
    let dir = [ "lib" ] / (if ocaml_version < (4,2) then "401" else "402") in
    unit "as_ocaml_incl" ~deps:[bytecomp] ~kind ~dir
  in
  let dir = ["driver-make"] in
  let unit ?deps ?(kind = kind) name = unit ?deps ~kind name ~dir in
  lib "assemblage_driver_make" ~deps:[cmdliner]
    [ unit "as_shell";
      unit "as_makefile";
      unit "as_cstubs";
      unit "as_ocamlfind";
      unit "as_pkg_config";
      unit "as_makefile";
      unit "as_project_makefile";
      as_ocaml_incl;
      unit "as_ocaml" ~deps:[bytecomp];
      unit "as_opam";
      unit "as_merlin";
      unit "assemblage_env";
      unit "as_setup_env";
      unit "as_setup";
      unit "as_tool";
      unit "assemblage_cmd";
      unit "assemblage" ~kind:(`OCaml (`Both, `Normal)); ]

let assemblage_tool =
  let u = unit "tool" ~dir:["bin"] ~deps:[toplevel] in
  bin "assemblage" [u] ~deps:[lib_assemblage; lib_driver_make]
    ~args:Args.linkall ~native:false

let ctypes_gen =
  let u = unit "ctypes_gen" ~dir:["bin"] in
  bin "ctypes-gen" [u] ~deps:[lib_assemblage] ~native:false

let assemble_assemble =
  (* Sanity check, can we compile assemble.ml to native code ? *)
  let u = unit "assemble" in
  bin "assemble" [u] ~deps:[lib_assemblage; lib_driver_make]

(* Tests & examples *)

let mk_test ?(example = false) name =
  let dir = [(if example then "examples" else "test"); name] in
  let args cmd env _ =
    let libdir = Product.dirname (List.hd (Part.products env lib_assemblage)) in
    [ cmd; "--auto-load=false"; "-I"; Path.to_string libdir; ]
  in
  run ~cond:Conf.(value test) name ~dir @@ fun env ->
  [ Part.Bin.cmd assemblage_tool (args "describe" env);
    Part.Bin.cmd assemblage_tool (args "setup" env);
    Rule.cmd ["make"];
    Rule.cmd ["make distclean"]; ]

let mk_example = mk_test ~example:true

let tests =
  [ mk_example "hello";
    mk_example "camlp4";
    mk_example "multi-libs";
    mk_example "containers";
    mk_example "pack";
    mk_example "threads";
    mk_example "threads-lib";
    mk_example "ctypes-libffi"; ]

(* Docs *)

let dev_doc = doc ~keep:Part.Doc.dev "dev" [lib_assemblage]
let api_doc = doc "api" [lib_assemblage]

(* The project *)

let p =
  Project.create "assemblage" @@
  [ lib_assemblage;
    assemblage_tool; assemble_assemble; ctypes_gen; dev_doc; api_doc ] @
  tests

let () = assemble p
