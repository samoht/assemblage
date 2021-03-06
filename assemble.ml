open Assemblage

(* Configuration schemes *)

let dev = Conf.scheme "dev" ~doc:"Convenience scheme for development."
    [ Conf.def Conf.debug true;
      Conf.def Conf.warn_error true;
      Conf.def Conf.doc true;]

let schemes = [ dev ]

(* Arguments *)

let args = Args.empty

(* OCamlfind packages *)

let pkg_cmdliner = pkg "cmdliner"
let pkg_bytecomp = pkg "compiler-libs.bytecomp"
let pkg_toplevel = pkg "compiler-libs.toplevel"
let pkg_bytes = pkg "bytes"

(* Libraries *)

let lib_assemblage =
  let dir = root / "lib" in
  let kind = `OCaml (`Both, `Hidden) in
  let unit ?needs ?(kind = kind) name = unit ?needs ~kind name ~dir in
  lib "assemblage"
    [ unit "as_univ";
      unit "as_string";
      unit "as_fmt";
      unit "as_path";
      unit "as_log";
      unit "as_cmd" ~needs:[pkg_bytes];
      unit "as_conf";
      unit "as_ctx";
      unit "as_args";
      unit "as_acmd";
      unit "as_action";
      unit "as_action_ocaml";
      unit "as_ocamlfind";
      unit "as_pkg_config";
      unit "as_part";
      unit "as_part_lib";
      unit "as_part_pkg";
      unit "as_part_unit";
      unit "as_part_doc";
      unit "as_part_bin";
      unit "as_part_dir";
      unit "as_part_run";
      unit "as_project";
      unit "assemblage" ~kind:(`OCaml (`Both, `Normal)) ]

let lib_assemblage_tools =
  let dir = root / "lib-driver" in
  let kind = `OCaml (`Both, `Hidden) in
  let unit ?needs ?(kind = kind) name = unit ?needs ~kind name ~dir in
  lib "assemblage_tools"
    [ lib_assemblage;
      unit "ast_merlin";
      unit "ast_meta";
      unit "ast_opam";
      unit "assemblage_tools" ~kind:(`OCaml (`Both, `Normal))]

let lib_assemblage_driver =
  let dir = root / "lib-driver" in
  lib "assemblage_driver" [
    pkg_cmdliner;
    pkg_toplevel;
    lib_assemblage;
    unit ~dir "assemblage_driver" ~needs:[pkg_cmdliner; pkg_toplevel];
  ]

(* The default assemblage driver *)

let bin_assemblage =
  let dir = root / "driver" in
  let unit ?needs ?kind name = unit ?needs ?kind name ~dir in
  bin "assemblage" ~native:false ~args:Args.linkall [
    pkg_cmdliner;
    pkg_toplevel;
    lib_assemblage;
    lib_assemblage_tools;
    lib_assemblage_driver;
    unit "makefile";
    unit "builder_makefile";
    unit "cmd_base" ~needs:[pkg_cmdliner];
    unit "cmd_build";
    unit "cmd_describe";
    unit "cmd_product";
    unit "cmd_help";
    unit "cmd_setup";
    unit "main" ~kind:(`OCaml (`Ml, `Normal));
  ]

(* Tests & examples *)

let assemble_assemble =
  (* Sanity check, can we compile assemble.ml ? *)
  bin "assemble" [ lib_assemblage; unit "assemble" ~dir:root ]

let mk_test ?(example = false) name =
  let dir = root / (if example then "examples" else "test") / name in
  let lib_dir = Conf.(value root_dir) // Part.root lib_assemblage in
  let make args = Acmd.v (Acmd.static "make") args in
  let cmds lib_dir assemblage =
    let assemblage sub args =
      let args = "--auto-lib=false" :: "-I" :: Path.to_string lib_dir :: args in
      Acmd.v assemblage (sub :: args)
    in
    [ assemblage "describe" [];
      assemblage "setup" [];
      make [];
      make ["distclean"]]
  in
  let cmds = Conf.(const cmds $ lib_dir) in
  Run.with_bin ~usage:`Test ~dir ~name bin_assemblage cmds

let mk_example = mk_test ~example:true

let tests =
  [ mk_test "builtin-keys";
    mk_example "hello";
    mk_example "gen-quine";
    mk_example "camlp4";
    mk_example "multi-libs";
    mk_example "containers";
    mk_example "pack";
    mk_example "threads";
    mk_example "threads-lib";
    mk_example "ctypes-libffi"; ]

(* Docs *)

let dev_doc = doc ~usage:`Dev "dev" [ lib_assemblage ]
let api_doc = doc "api" [ lib_assemblage ]

let install =
  [ dir `Lib [ lib_assemblage; lib_assemblage_tools; lib_assemblage_driver ];
    dir `Bin [ bin_assemblage ];
    dir `Doc [ file (Path.v "README.md"); file (Path.v "CHANGES.md") ]]

(* The project *)

let parts =
  [ lib_assemblage; lib_assemblage_tools; lib_assemblage_driver;
    bin_assemblage; dev_doc; api_doc ] @ install @ tests

let () = assemble @@ Project.v "assemblage" ~args ~schemes ~parts
