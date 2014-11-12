open Assemblage

(* Configuration schemes *)

let dev = Conf.scheme "dev" ~doc:"Convenience scheme for development."
    [ Conf.def Conf.debug true;
      Conf.def Conf.warn_error true;
      Conf.def Conf.doc true; ]

let schemes = [ dev ]

(* Arguments *)

let args = Args.empty

(* OCamlfind packages *)

let pkg_cmdliner = pkg "cmdliner"
let pkg_bytecomp = pkg "compiler-libs.bytecomp"
let pkg_toplevel = pkg "compiler-libs.toplevel"

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
      unit "as_cmd";
      unit "as_conf" ~needs:[pkg_cmdliner]; (* FIXME remove dep *)
      unit "as_ctx";
      unit "as_args";
      unit "as_action";
      unit "as_action_ocaml";
      unit "as_part";
      unit "as_part_bin";
      unit "as_part_custom";
      unit "as_part_dir";
      unit "as_part_doc";
      unit "as_part_lib";
      unit "as_part_pkg";
      unit "as_part_run";
      unit "as_part_unit";
      unit "as_project";
      unit "assemblage" ~kind:(`OCaml (`Both, `Normal)); ]

let lib_assemblage_tools =
  let dir = root / "lib-driver" in
  let kind = `OCaml (`Both, `Hidden) in
  let unit ?needs ?(kind = kind) name = unit ?needs ~kind name ~dir in
  lib "assemblage_tools"
    [ unit "ast_merlin";
      unit "ast_meta";
      unit "ast_opam";
      unit "assemblage_tools" ~kind:(`OCaml (`Both, `Normal)); ]

let lib_assemblage_driver =
  let dir = root / "lib-driver" in
  lib "assemblage_driver"
    [ pkg_cmdliner;
      pkg_toplevel;
      unit ~dir "assemblage_driver"; ]

(* The default assemblage driver *)

let bin_assemblage =
  let dir = root / "driver" in
  let unit ?needs ?kind name = unit ?needs ?kind name ~dir in
  bin "assemblage" ~native:false ~args:Args.linkall
    [ pkg_cmdliner;
      lib_assemblage;
      lib_assemblage_tools;
      lib_assemblage_driver;
      unit "builder_makefile";
      unit "cmd_base";
      unit "cmd_build";
      unit "cmd_describe";
      unit "cmd_help";
      unit "cmd_setup";
      unit "main" ~kind:(`OCaml (`Ml, `Normal));
      unit "makefile"; ]

(* Tests & examples *)

let assemble_assemble =
  (* Sanity check, can we compile assemble.ml ? *)
  bin "assemble" [lib_assemblage; unit "assemble" ~dir:root ]

(*

let mk_test ?(example = false) name =
  let open Action.Spec in
  let dir = root / (if example then "examples" else "test") / name in
  let lib_dir =
    let get_dir prods = Path.dirname (List.hd prods) in
    Conf.(const get_dir $ Part.products lib_assemblage)
  in
  let assemblage_cmd = Bin.cmd bin_assemblage in
  let assemblage cmd args =
    let args = add (atom cmd) :: add (atom "--auto-lib=false") ::
               path_arg ~opt:"-I" lib_dir :: args
    in
    Action.cmd assemblage_cmd args
  in
  let cmds =
    Action.(assemblage "describe" <*>
            assemblage "setup" <*>
            cmd Conf.make (atom []) <*>
            cmd Conf.make (atom "distclean"))
  in
  let action = Action.v ~ctx:Ctx.empty ~inputs:(Conf.value assemblage_cmd)
      cmds
  in
  run ~usage:`Test ~cond action


let mk_example = mk_test ~example:true
*)
let tests = []
(*
  [ mk_test "builtin-keys"
    mk_example "hello";
    mk_example "camlp4";
    mk_example "multi-libs";
    mk_example "containers";
    mk_example "pack";
    mk_example "threads";
    mk_example "threads-lib";
    mk_example "ctypes-libffi"; ]
*)

(* Docs *)

let dev_doc = doc ~usage:`Dev "dev" [lib_assemblage]
let api_doc = doc "api" [lib_assemblage]

let install =
  [ dir `Lib [ lib_assemblage; lib_assemblage_tools; lib_assemblage_driver; ];
    dir `Bin [ bin_assemblage ] ;
(* FIXME   dir `Doc [ root_file "CHANGES.md"; root_file "README.md" ] *) ]

let parts =
  [ lib_assemblage; lib_assemblage_tools; lib_assemblage_driver;
    bin_assemblage; dev_doc; api_doc ] @ install @ tests

(* The project *)

let () = assemble @@ Project.v "assemblage" ~args ~schemes ~parts
