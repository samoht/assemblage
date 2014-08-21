(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

(* Features and flags *)

let (/) = Filename.concat

module Features = As_features
module Flags = As_flags

(* Resolvers and actions *)

module Resolver = As_resolver
module Action = As_action

(* Components *)

type project = As_project.t
type component = As_component.component
type comp_unit = As_component.comp_unit
type other = As_component.other
type pkg = As_component.pkg
type lib = As_component.lib
type bin = As_component.bin
type container = As_component.container
type test = As_component.test
type doc = As_component.doc

let unit ?available ?flags ?deps ?interface name origin =
  `Unit (As_component.Unit.create ?available ?flags ?deps ?interface
           name `OCaml origin)

let pack ?available ?flags ?deps name units =
  let units = List.map (function `Unit u -> u) units in
  `Unit (As_component.Unit.pack ?available ?flags ?deps name units)

let c ?available ?(flags=As_flags.empty) ?deps ?(cclib = []) ?(ccopt = [])
    name origin =
  let flags =
    let (@@@) = As_flags.(@@@) in
    flags @@@ As_flags.ccopt ccopt @@@ As_flags.cclib cclib in
  `Unit (As_component.Unit.create ?available ~flags ?deps name `C origin)

let js ?available ?(flags=As_flags.empty) ?deps ?(jsflags = []) name origin =
  let flags =
    let (@@@) = As_flags.(@@@) in
    flags @@@ As_flags.v (`Link `Js) jsflags in
  `Unit (As_component.Unit.create ?available ~flags ?deps name `Js origin)

let other ?available ?flags ?deps name action =
  `Other (As_component.Other.create ?available ?flags ?deps name action)

let pkg ?available ?flags ?opt name =
  `Pkg (As_component.Pkg.create ?available ?flags ?opt name `OCaml)

let pkg_pp ?available ?flags ?opt name =
  `Pkg (As_component.Pkg.create ?available ?flags ?opt name `OCaml_pp)

let pkg_c ?available ?flags ?opt name =
  `Pkg (As_component.Pkg.create ?available ?flags ?opt name `C)

let lib ?available ?flags ?deps ?byte ?native ?native_dynlink ?pack name
    origin =
  `Lib (As_component.Lib.create ?available ?flags ?deps ?byte ?native
          ?native_dynlink ?pack name `OCaml origin)

let lib_pp ?available ?flags ?deps ?byte ?native ?native_dynlink ?pack name
    origin =
  `Lib (As_component.Lib.create ?available ?flags ?deps ?byte ?native
          ?native_dynlink ?pack name `OCaml_pp origin)

let bin ?available ?flags ?deps ?byte ?native ?js ?linkall ?install name units =
  `Bin (As_component.Bin.create ?available ?flags ?deps ?byte ?native ?js
          ?linkall ?install name units)

let container ?available ?flags ?deps name contents =
  `Container (As_component.Container.create ?available ?flags ?deps name
                contents)

let doc ?available ?flags ?deps ?install name contents =
  `Doc (As_component.Doc.create ?available ?flags ?deps ?install name contents)

type test_command = As_component.Test.command

let test ?available ?flags ?deps ?dir name commands =
  `Test (As_component.Test.create ?available ?flags ?deps ?dir name commands)

type test_args = As_component.Test.args

let test_bin bin ?args (): As_component.Test.command =
  let args = match args with
  | None   -> (fun _ -> [])
  | Some a -> a
  in
  `Bin (bin, args)

let test_shell fmt =
  ksprintf (fun str -> `Shell str) fmt

(* Component helpers *)

let pick name c =
  List.find
    (fun c -> As_component.name c = name)
    (As_component.contents c)

let build_dir = As_component.build_dir
let root_dir = As_resolver.root_dir

let cstubs ?available ?(deps = []) ?(headers = []) ?(cflags = []) ?(clibs = [])
    name (`Path path)
  =
  let name_bindings = name ^ "_bindings" in
  let name_stubs = name ^ "_stubs" in

  (* 1. compile the bindings. *)
  let deps = `Pkg As_component.Pkg.ctypes_stub :: deps in
  let bindings = unit name_bindings (`Path path) ~deps in

  (* 2. compile the generator of <name>_stubs.{ml,c} and <name>.ml *)
  let generator =
    let name_generator = name ^ "_generator" in
    let ctypes_gen =
      other (name ^ "-generator") [
        As_action.rule
          ~phase:`Prepare
          ~targets:[`Self `Ml]
          ~prereqs:[]
          (fun _t r _f ->
             let dir = As_component.build_dir bindings r in
             let ml_stubs = dir / name_stubs ^ ".ml" in
             let c_stubs  = dir / name_stubs ^ ".c" in
             let library  = dir / name ^ ".ml" in
             let headers = match headers with
             | [] -> ""
             | hs -> sprintf "--headers %s " (String.concat "," hs) in
             As_action.create ~dir
               "ctypes-gen %s--ml-stubs %s --c-stubs %s --library %s %s"
               headers ml_stubs c_stubs library name)
      ] in
    let unit = unit name_generator ctypes_gen in
    bin name_generator (`Units [unit])
  in

  (* 3. compile the generated stubs *)
  let run_generator =
    other (name ^ "-generator.run") [
      As_action.rule
        ~phase:`Prepare
        ~targets:[`Self `Ml; `Self `C]
        ~prereqs:[`N (generator, `Byte)]
        (fun t r _f ->
           let dir = As_component.build_dir t r in
           As_action.create ~dir "./%s.byte" (As_component.name t))
    ] in
  let ml_stubs = unit name_stubs run_generator ~deps:[bindings] in
  let c_stubs = c name_stubs run_generator in
  let main = unit name run_generator ~deps:[ml_stubs; c_stubs] in

  (* 4. compile the main library *)
  let flags =
    let link_flags = cflags @ List.map (sprintf "-l%s") clibs in
    As_flags.(cclib link_flags @@@ stub name_stubs) in
  (* FIXME: which action ? *)
  lib name ~flags ?available (`Units [bindings; ml_stubs; c_stubs; main])

(* Projects *)

let project = As_project.create

(* Tools *)

module Build_env = As_build_env
module Cmd = As_cmd
let assemble = As_cmd.assemble
