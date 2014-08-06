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

type t = As_project.t
type component = As_project.component
type comp_unit = As_project.comp_unit
type other = As_project.other
type pkg = As_project.pkg
type lib = As_project.lib
type bin = As_project.bin
type dir = As_project.dir
type test = As_project.test
type doc = As_project.doc

type file = {
  name: string;
  available: As_features.t;
  flags: As_flags.t;
  deps: component list;
}

let unit ?available ?flags ?deps name origin =
  `Unit (As_project.Unit.create ?available ?flags ?deps name `OCaml origin)

let c ?available ?(flags=As_flags.empty) ?deps ?(cclib = []) ?(ccopt = []) name origin =
  let flags =
    let (@@@) = As_flags.(@@@) in
    flags @@@ As_flags.ccopt ccopt @@@ As_flags.cclib cclib in
  `Unit (As_project.Unit.create ?available ~flags ?deps name `C origin)

let js ?available ?(flags=As_flags.empty) ?deps ?(jsflags = []) name origin =
  let flags =
    let (@@@) = As_flags.(@@@) in
    flags @@@ As_flags.v (`Link `Js) jsflags in
  `Unit (As_project.Unit.create ?available ~flags ?deps name `Js origin)

let other ?available ?flags ?deps name action =
  `Other (As_project.Other.create ?available ?flags ?deps name action)

let pkg ?available ?flags ?opt name =
  `Pkg (As_project.Pkg.create ?available ?flags ?opt name `OCaml)

let pkg_pp ?available ?flags ?opt name =
  `Pkg (As_project.Pkg.create ?available ?flags ?opt name `OCaml_pp)

let pkg_c ?available ?flags ?opt name =
  `Pkg (As_project.Pkg.create ?available ?flags ?opt name `C)

let lib ?available ?flags ?deps ?pack name origin =
  `Lib (As_project.Lib.create ?available ?flags ?deps ?pack name `OCaml origin)

let lib_pp ?available ?flags ?deps ?pack name origin =
  `Lib (As_project.Lib.create ?available ?flags ?deps ?pack name `OCaml_pp origin)

let bin ?available ?flags ?deps ?byte ?native ?js ?link_all ?install name units =
  `Bin (As_project.Bin.create ?available ?flags ?deps ?byte ?native ?js ?link_all
          ?install name units)

let dir ?available ?flags ?deps ?install name contents =
  `Dir (As_project.Dir.create ?available ?flags ?deps ?install name contents)

type test_command = As_project.Test.command

let test ?available ?flags ?deps ?dir name commands =
  `Test (As_project.Test.create ?available ?flags ?deps ?dir name commands)

type test_args = As_project.Test.args

let test_bin bin ?args (): As_project.Test.command =
  let args = match args with
  | None   -> (fun _ -> [])
  | Some a -> a
  in
  `Bin (bin, args)

let test_shell fmt =
  ksprintf (fun str -> `Shell str) fmt

(* Component helpers *)

let build_dir = As_project.Component.build_dir

let cstubs ?available ?(deps = []) ?(headers = []) ?(cflags = []) ?(clibs = [])
    name (`Dir dir)
  =
  let name_bindings = name ^ "_bindings" in
  let name_stubs = name ^ "_stubs" in

  (* 1. compile the bindings. *)
  let deps = `Pkg As_project.Pkg.ctypes_stub :: deps in
  let bindings = unit name_bindings (`Dir dir) ~deps in

  (* 2. compile the generator of <name>_stubs.{ml,c} and <name>.ml *)
  let generator =
    let name_generator = name ^ "_generator" in
    let ctypes_gen =
      other (name ^ "-generator") [
        As_action.rule
          ~phase:`Prepare
          ~targets:[`Self `Ml]
          ~prereqs:[]
          (fun t r f ->
             let dir = As_project.Component.build_dir bindings r in
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
        (fun t r f ->
           let dir = As_project.Component.build_dir t r in
           As_action.create ~dir "./%s.byte" (As_project.Component.name t))
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

let project_list = ref []
let projects () = !project_list
let add t = project_list := t :: !project_list
let create = As_project.create

(* Tools *)

module Build_env = As_build_env

type tool = t -> As_build_env.t -> unit

let sys_argl = Array.to_list Sys.argv

let auto_load () =
  List.for_all ((<>) "--disable-auto-load") sys_argl

let includes () =
  let rec aux acc = function
    | []             -> List.rev acc
    | "-I" :: h :: t -> aux (h::acc) t
    | _ :: t         -> aux acc t in
  aux [] sys_argl


let configure `Make t env =
  let features = As_build_env.features env in
  let flags = As_build_env.flags env in
  let makefile = "Makefile" in
  let build_dir = As_build_env.build_dir env in
  As_makefile.(write (of_project t ~features ~flags ~makefile));
  As_ocamlfind.META.(write (of_project t));
  As_opam.Install.(write (of_project ~build_dir t))

let describe t env =
  let print_deps x =
    let bold_name pkg = As_shell.color `Bold (As_project.Pkg.name pkg) in
    let pkgs = As_project.Component.(filter pkg x) in
    match String.concat " " (List.map bold_name pkgs) with
    | "" -> ""
    | pkgs -> sprintf "  ├─── [%s]\n" pkgs
  in
  let print_modules last modules =
    let aux i n m =
      printf "  %s %s\n"
        (if last && i = n then "└───" else "├───") (As_shell.color `Blue m) in
    let n = List.length modules - 1 in
    List.iteri (fun i m -> aux i n m) modules in
  let print_units units =
    let aux i n u =
      let mk f ext =
        if f u then (As_shell.color `Cyan (As_project.Unit.name u ^ ext)) else
        ""
      in
      let ml = mk As_project.Unit.(has `Ml) ".ml" in
      let mli = mk As_project.Unit.(has `Mli) ".mli" in
      let modules =
        if As_project.Unit.generated u then ["--generated--"]
        else
          let build_dir = As_build_env.build_dir env in
          As_OCaml.modules ~build_dir u in
      printf "  %s %-25s%-25s\n"
        (if modules = [] && i = n then "└─" else "├─") ml mli;
      print_modules (i=n) modules
    in
    let n = List.length units - 1 in
    List.iteri (fun i u -> aux i n u) units in
  let print cs =
    let aux c =
      let open As_project.Component in
      printf "└─┬─ %s\n%s"
        (As_shell.color `Magenta (id c)) (print_deps (deps c));
      print_units (filter unit (contents c)) in
    List.iter aux cs in
  printf "\n%s %s %s\n\n"
    (As_shell.color `Yellow "==>")
    (As_shell.color `Underline (As_project.name t)) (As_project.version t);
  let components =
    As_project.components t
    |> List.filter (function `Unit _ | `Lib _ | `Bin _ -> true | _ -> false)
  in
  print components

let process ?(file = "assemble.ml") name fn =
  let includes = includes () in
  let auto_load = auto_load () in
  As_shell.show "Loading %s. %s"
    (As_shell.color `Bold file)
    (if auto_load then "" else
       sprintf "[auto-load: %s]"
         (As_shell.color `Magenta (string_of_bool auto_load)));
  Toploop.initialize_toplevel_env ();
  Toploop.set_paths ();
  let includes =
    if auto_load then
      includes @ As_shell.exec_output "ocamlfind query -r assemblage"
    else
      includes in
  List.iter Topdirs.dir_directory includes;
  if not (Sys.file_exists file) then
    As_shell.fatal_error 1 "missing %s." file
  else match Toploop.use_silently Format.err_formatter file with
    | false -> As_shell.fatal_error 1 "while loading `%s'." file
    | true  ->
      match projects () with
      | [] -> As_shell.fatal_error 2 "No projects are registered in `%s'." file
      | ts ->
        let features = List.fold_left (fun acc t ->
            As_features.Set.union (As_project.features t) acc
          ) As_features.Set.empty ts in
        let features =
          As_features.Set.fold (fun f acc ->
              As_features.(acc ||| atom f)
            ) features As_features.false_ in
        let env = As_build_env.parse name features in
        List.iter (fun t ->
            As_shell.show "Project: %s" (As_project.name t);
            describe t env;
            fn t env
          ) ts
