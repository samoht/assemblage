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
open Project

let project_list = ref []

type t = Project.t
type component = Component.t
type cu = CU.t
type lib = Lib.t
type bin = Bin.t
type gen = Gen.t
type c = C.t
type js = JS.t
type test = Test.t

let projects () =
  !project_list

let create ?flags ?doc_css ?doc_intro ?doc_dir ?version components name =
  let t =
    Project.create ?flags ?doc_css ?doc_intro ?doc_dir ?version components name in
  project_list := t :: !project_list

let cu ?dir deps name =
  let u = CU.create ?dir ~deps name in
  `CU u

let ocamldep ~dir ?flags deps =
  let resolver = Ocamlfind.resolver `Direct (Sys.getcwd ()) in
  let cus = OCaml.depends ?flags ~deps resolver dir in
  List.map (fun cu -> `CU cu) cus

let generated ?action deps f name =
  let g = Gen.create ~deps ?action f name in
  `Gen g

let lib ?available ?(flags=Flags.empty) ?pack ?(deps=[])
    (cus:[`CU of cu] list) name =
  let g = Component.Graph.create () in
  List.iter (fun (`CU cu) ->
      Component.Graph.add_vertex g (`CU cu);
      List.iter (function
          | `CU dep ->
            if List.mem (`CU dep) cus then
              Component.Graph.add_edge g (`CU dep) (`CU cu)
          | _ -> ()
        ) (CU.deps cu)
    ) cus;
  let cus = Component.Graph.vertex g in
  let cus = List.map (function `CU cu -> cu | _ -> assert false) cus in
  let l = Lib.create ?available ~flags ?pack ~deps cus name in
  `Lib l

let bin ?(dir="bin") ?byte_only ?link_all ?install deps files name =
  let cus = List.map (CU.create ~dir ~deps) files in
  let b = Bin.create ?byte_only ?link_all ?install cus name in
  `Bin b

let c ?(dir="stubs") ?(link_flags=[]) deps libs name =
  let link_flags = List.map (sprintf "-l%s") libs @ link_flags in
  let c = C.create ~dir ~link_flags ~deps name in
  `C c

let js b r =
  let `Bin b = b in
  `JS (JS.create b r)

let pkg x = `Pkg x

let pkg_pp x = `Pkg_pp x

let test ?dir deps commands name =
  `Test (Test.create ?dir ~deps commands name)

let test_bin (`Bin bin) args: Test.command =
  `Bin (`Bin bin, args)

let test_shell fmt =
  ksprintf (fun str -> `Shell str) fmt

(* Ctypes stub-generation *)

let cstubs ?dir ?available ?(headers=[]) ?(cflags=[]) ?(clibs=[]) deps name =

  (* 1. compile the bindings. *)
  let deps = `Pkg "ctypes.stubs" :: deps in
  let name_bindings = name ^ "_bindings" in
  let bindings = cu ?dir deps name_bindings in

  (* 2. Generate and compile the generator. *)
  let name_generator = name ^ "_generator" in
  let generator =
    let action r =
      let headers = match headers with
        | [] -> ""
        | hs -> sprintf "--headers %s " (String.concat "," hs) in
      Action.create ~dir:(Resolver.build_dir r "") "ctypes-gen %s" headers
    in
    let gen = generated ~action [] `ML name_generator in
    let comp = CU.create ~deps:[gen; bindings] name_generator in
    let bin =
      Bin.create ~install:false [comp] name_generator in
    `Bin bin in

  (* 3. Generate and compile the stubs. *)
  let name_stubs = name ^ "_stubs" in
  let ml_stubs =
    let action r =
      Action.create ~dir:(Resolver.build_dir r "") "./%s.byte" name_generator in
    let gen = generated ~action [generator] `ML name_stubs in
    cu [gen] name_stubs in
  let link_flags = cflags @ List.map (sprintf "-l%s") clibs in
  let c_stubs =
    let c = C.create ~generated:true ~deps:[generator] ~link_flags name_stubs in
    `C c in
  let flags = Flags.(cclib link_flags @ stub name_stubs) in
  let gen = generated [generator] `ML name in
  let main = cu [bindings; ml_stubs; c_stubs; gen] name in
  lib ~flags ?available [bindings; ml_stubs; main] name

type tool = t -> Build_env.t -> unit

let sys_argl = Array.to_list Sys.argv

let auto_load () =
  List.for_all ((<>) "--disable-auto-load") sys_argl

let includes () =
  let rec aux acc = function
    | []             -> List.rev acc
    | "-I" :: h :: t -> aux (h::acc) t
    | _ :: t         -> aux acc t in
  aux [] sys_argl

let process ?(file="configure.ml") name fn =
  let includes = includes () in
  let auto_load = auto_load () in
  Shell.show "Loading %s. %s"
    (Shell.color `bold file)
    (if auto_load then "" else
       sprintf "[auto-load: %s]"
         (Shell.color `magenta (string_of_bool auto_load)));
  Toploop.initialize_toplevel_env ();
  Toploop.set_paths ();
  let includes =
    if auto_load then
      includes @ Shell.exec_output "ocamlfind query -r assemblage"
    else
      includes in
  List.iter Topdirs.dir_directory includes;
  if not (Sys.file_exists file) then
    Shell.fatal_error 1 "missing %s." file
  else match Toploop.use_silently Format.std_formatter file with
    | false -> Shell.fatal_error 1 "while loading `%s'." file
    | true  ->
      match projects () with
      | [] -> Shell.fatal_error 2 "No projects are registered in `%s'." file
      | ts ->
        let features = List.fold_left (fun acc t ->
            Feature.Set.union (Project.features t) acc
          ) Feature.Set.empty ts in
        let env = Build_env.parse name features in
        List.iter (fun t -> fn t env) ts

let configure `Make t env =
  let features = Build_env.features env in
  let flags = Build_env.flags env in
  let makefile = "Makefile" in
  let build_dir = Build_env.build_dir env in
  Makefile.(write @@ of_project t ~features ~flags ~makefile);
  Ocamlfind.META.(write @@ of_project t);
  Opam.Install.(write @@ of_project ~build_dir t)

let describe t env =
  let print_deps x = match Component.(filter pkg x @ filter pkg_pp x) with
    | [] -> ""
    | ds -> sprintf "  ├─── [%s]\n"
              (String.concat " " (List.map (Shell.color `bold) ds)) in
  let print_modules last ms =
    let aux i n m =
      printf "  %s %s\n"
        (if last && i = n then "└───" else "├───") (Shell.color `blue m) in
    let n = List.length ms - 1 in
    List.iteri (fun i m -> aux i n m) ms in
  let print_units us =
    let aux i n u =
    let mk f ext =
      if f u then (Shell.color `cyan @@ CU.name u ^ ext) else "" in
    let ml = mk CU.ml ".ml" in
    let mli = mk CU.mli ".mli" in
    printf "  %s %-25s%-25s\n" (if i = n then "└─" else "├─") ml mli;
    let build_dir = Build_env.build_dir env in
    print_modules (i=n) (OCaml.modules ~build_dir u)
    in
    let n = List.length us - 1 in
    List.iteri (fun i u -> aux i n u) us in
  let print_top id deps comps ls =
    let aux l =
      printf "└─┬─ %s\n%s"
        (Shell.color `magenta (id l)) (print_deps @@ deps l);
      print_units (comps l) in
    List.iter aux ls in
  let print_libs = print_top Lib.id Lib.deps Lib.compilation_units in
  let print_bins = print_top Bin.id Bin.deps Bin.compilation_units in
  printf "\n%s %s %s\n\n"
    (Shell.color `yellow "==>")
    (Shell.color `underline (Project.name t)) (Project.version t);
  let components = Project.components t in
  print_libs Component.(filter lib components);
  print_libs Component.(filter pp  components);
  print_bins Component.(filter bin components)
