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

type t = As_project.t
type component = As_project.Component.t
type comp_unit = As_project.Unit.t
type lib = As_project.Lib.t
type bin = As_project.Bin.t
type pkg = As_project.Pkg.t
type gen = As_project.Gen.t
type c = As_project.C.t
type js = As_project.JS.t
type test = As_project.Test.t

let project_list = ref []
let projects () = !project_list

let create ?flags ?doc_css ?doc_intro ?doc_dir ?doc_public ?version name
    components =
  let t =
    As_project.create ?flags ?doc_css ?doc_intro ?doc_dir ?doc_public ?version
      components name in
  project_list := t :: !project_list

let unit ?available ?dir name deps =
  `Unit (As_project.Unit.create ?available ?dir ~deps name)

let ocamldep ~dir ?flags deps =
  let resolver = As_ocamlfind.resolver `Direct (Sys.getcwd ()) in
  let cus = As_OCaml.depends ?flags ~deps resolver dir in
  List.map (fun cu -> `Unit cu) cus

let generated ?action name deps f =
  let g = As_project.Gen.create ~deps ?action f name in
  `Gen g

let nil _ = []

let sorted_cus cus =
  let g = As_project.Component.Graph.create () in
  List.iter (fun (`Unit cu) ->
      As_project.Component.Graph.add_vertex g (`Unit cu);
      List.iter (function
          | `Unit dep ->
            if List.mem (`Unit dep) cus then
              As_project.Component.Graph.add_edge g (`Unit dep) (`Unit cu)
          | _ -> ()
        ) (As_project.Unit.deps cu)
    ) cus;
  let cus = As_project.Component.Graph.to_list g in
  List.map (function `Unit cu -> cu | _ -> assert false) cus

let lib ?available ?(flags=As_flags.empty) ?pack ?(deps=nil) ?(c=[])
    name (cus:[`Unit of comp_unit] list) =
  let cus = sorted_cus cus in
  let c = List.map (function `C c -> c) c in
  let l = As_project.Lib.create ?available ~flags ?pack ~deps ~c cus name in
  `Lib l

let bin ?byte_only ?link_all ?install ?(deps=nil) name cus =
  let cus = sorted_cus cus in
  let b = As_project.Bin.create ?byte_only ?link_all ?install ~deps cus name in
  `Bin b

let c ?dir ?(link_flags=[]) name deps libs =
  let link_flags = List.map (sprintf "-l%s") libs @ link_flags in
  let c = As_project.C.create ?dir ~link_flags ~deps name in
  `C c

let js b r =
  let `Bin b = b in
  `JS (As_project.JS.create b r)

let pkg ?available ?opt name =
  `Pkg (As_project.Pkg.create ?available ?opt name ~is_pp:false)

let pkg_pp ?available ?opt name =
  `Pkg_pp (As_project.Pkg.create ?available ?opt name ~is_pp:false)

let test ?dir name deps commands =
  `Test (As_project.Test.create ?dir ~deps commands name)

type test_args = (component -> string) -> string list

let test_bin (`Bin bin) ?args (): As_project.Test.command =
  let args = match args with
    | None   -> (fun _ -> [])
    | Some a -> a in
  `Bin (`Bin bin, args)

let test_shell fmt =
  ksprintf (fun str -> `Shell str) fmt

(* Ctypes stub-generation *)
let (/) = Filename.concat

let cstubs ?dir ?available ?(headers=[]) ?(cflags=[]) ?(clibs=[]) name deps =

  (* 1. compile the bindings. *)
  let deps = `Pkg As_project.Pkg.ctypes_stub :: deps in
  let name_bindings = name ^ "_bindings" in
  let bindings = As_project.Unit.create ?dir ~deps:deps name_bindings in

  let name_generator = name ^ "_generator" in
  let name_stubs = name ^ "_stubs" in
  let bin_dir r = As_project.Bin.build_dir
      (As_project.Bin.create [] name_generator) r in
  let lib_dir r =
    Sys.getcwd () / As_project.Lib.build_dir (As_project.Lib.create [] name) r
  in

  (* 2. Generate and compile the generator. *)
  let generator =
    let action r =
      let ml_stubs = lib_dir r / name_stubs ^ ".ml" in
      let c_stubs  = lib_dir r / name_stubs ^ ".c" in
      let library  = lib_dir r / name ^ ".ml" in
      let headers = match headers with
        | [] -> ""
        | hs -> sprintf "--headers %s " (String.concat "," hs) in
      As_action.custom ~dir:(bin_dir r)
        "ctypes-gen %s--ml-stubs %s --c-stubs %s --library %s %s"
        headers ml_stubs c_stubs library name
    in
    let ml = generated name_generator ~action [] [`Ml] in
    let comp =
      As_project.Unit.create ~deps:[ml; `Unit bindings] name_generator
    in
    let bin =
      As_project.Bin.create ~install:false [bindings; comp] name_generator
    in
    `Bin bin in

  (* 3. Generate and compile the stubs. *)
  let ml_stubs =
    let action r = As_action.custom ~dir:(bin_dir r) "./%s.byte" name_generator
    in
    let ml = generated name_stubs ~action [generator] [`C; `Ml] in
    unit name_stubs [ml] in

  let link_flags = cflags @ List.map (sprintf "-l%s") clibs in
  let c_stubs =
    let c = As_project.C.create ~generated:true ~deps:[generator] ~link_flags
        name_stubs in
    `C c in
  let flags = As_flags.(cclib link_flags @@@ stub name_stubs) in
  let ml = generated name [generator] [`Ml] in
  let main = unit name [`Unit bindings; ml_stubs; c_stubs; ml] in
  lib name ~flags ?available ~c:[c_stubs] [`Unit bindings; ml_stubs; main]

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

let process ?(file="assemble.ml") name fn =
  let includes = includes () in
  let auto_load = auto_load () in
  As_shell.show "Loading %s. %s"
    (As_shell.color `bold file)
    (if auto_load then "" else
       sprintf "[auto-load: %s]"
         (As_shell.color `magenta (string_of_bool auto_load)));
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
  else match Toploop.use_silently Format.std_formatter file with
    | false -> As_shell.fatal_error 1 "while loading `%s'." file
    | true  ->
      match projects () with
      | [] -> As_shell.fatal_error 2 "No projects are registered in `%s'." file
      | ts ->
        let features = List.fold_left (fun acc t ->
            As_features.Set.union (As_project.features t) acc
          ) As_features.Set.empty ts in
        let env = As_build_env.parse name features in
        List.iter (fun t -> fn t env) ts

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
    let bold_name pkg = As_shell.color `bold (As_project.Pkg.name pkg) in
    let pkgs = As_project.Component.(filter pkg x @ filter pkg_pp x) in
    match String.concat " " (List.map bold_name pkgs) with
    | "" -> ""
    | pkgs -> sprintf "  ├─── [%s]\n" pkgs
  in
  let print_modules last ms =
    let aux i n m =
      printf "  %s %s\n"
        (if last && i = n then "└───" else "├───") (As_shell.color `blue m) in
    let n = List.length ms - 1 in
    List.iteri (fun i m -> aux i n m) ms in
  let print_units us =
    let aux i n u =
      let mk f ext =
        if f u then (As_shell.color `cyan (As_project.Unit.name u ^ ext)) else
        ""
      in
      let ml = mk As_project.Unit.ml ".ml" in
      let mli = mk As_project.Unit.mli ".mli" in
      let modules =
        if As_project.Unit.generated u then ["--generated--"]
        else
          let build_dir = As_build_env.build_dir env in
          As_OCaml.modules ~build_dir u in
      printf "  %s %-25s%-25s\n"
        (if modules = [] && i = n then "└─" else "├─") ml mli;
      print_modules (i=n) modules
    in
    let n = List.length us - 1 in
    List.iteri (fun i u -> aux i n u) us in
  let print_top id deps comps ls =
    let aux l =
      printf "└─┬─ %s\n%s"
        (As_shell.color `magenta (id l)) (print_deps (deps l));
      print_units (comps l) in
    List.iter aux ls in
  let print_libs = As_project.Lib.(print_top id deps units) in
  let print_bins = As_project.Bin.(print_top id deps units) in
  printf "\n%s %s %s\n\n"
    (As_shell.color `yellow "==>")
    (As_shell.color `underline (As_project.name t)) (As_project.version t);
  let components = As_project.components t in
  print_libs As_project.Component.(filter lib components);
  print_libs As_project.Component.(filter pp  components);
  print_bins As_project.Component.(filter bin components)

type test_command = As_project.Test.command

module Build_env = As_build_env
module Action = As_action
module Resolver = As_resolver
module Flags = As_flags
module Features = As_features
