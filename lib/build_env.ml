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
open Cmdliner
open Project

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Thomas Gazagnaire <thomas@gazagnaire.org>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ocaml-tools/issues.";
]

type global = {
  mutable verbose: bool;
}

let global = {
  verbose = false;
}

let debug fmt =
  ksprintf (fun str ->
      printf "+ %s\n" str
    ) fmt

let global =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Verbose mode." ["v";"verbose"] in
    Arg.(value & flag & doc) in
  let help =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Display help." ["h";"help"] in
    Arg.(value & flag & doc) in
  let create verbose help man_format =
    if help then `Help (man_format, None)
    else (
      global.verbose <- verbose;
      `Ok ()
    ) in
  Term.(ret (pure create $ verbose $ help $ Term.man_format))

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun () -> fn) $ global)

type t = {
  native: bool;
  native_dynlink: bool;
  features: (Feature.t * bool) list;
  comp: string list;
  bytcomp: string list;
  natcomp: string list;
  link: string list;
  bytlink: string list;
  natlink: string list;
  pp: string list;
  includes: string list;
  auto_load: bool;
  build_dir: string;
  name: string option;
  version: string option;
}

let create
    ?(native=true)
    ?(native_dynlink=true)
    ?(features=[])
    ?(comp=[])
    ?(bytcomp=[])
    ?(natcomp=[])
    ?(link=[])
    ?(bytlink=[])
    ?(natlink=[])
    ?(pp=[])
    ?(includes=[])
    ?(auto_load=true)
    ?(build_dir="_build")
    ?name ?version
    () =
  { native; native_dynlink; features; comp; bytcomp; natcomp;
    link; bytlink; natlink; pp; build_dir; auto_load; includes;
    name; version }

let build_dir t = t.build_dir
let native t = t.native
let native_dynlink t = t.native && t.native_dynlink
let features t = t.features

let comp t = t.comp
let bytcomp t = t.bytcomp @ t.comp
let natcomp t = t.natcomp @ t.comp

let link t = t.link
let bytlink t = t.bytlink @ t.link
let natlink t = t.natlink @ t.link

let pp t = t.pp

let default = {
  native = true;
  native_dynlink = true;
  features = [];
  comp = [];
  bytcomp = [];
  natcomp = [];
  link = [];
  bytlink = [];
  natlink = [];
  pp = [];
  auto_load = true;
  includes = [];
  build_dir = "_build";
  name = None;
  version = None;
}

let enable t flags =
  List.for_all (fun f ->
      try List.assoc f t.features
      with Not_found -> false
    ) flags

let term_of_list list =
  let aux acc h = Term.(pure (fun f t -> f :: t) $ h $ acc) in
  List.fold_left aux (Term.pure []) list

let term flags =
  let flags = Feature.Set.elements flags in
  let flags = term_of_list (List.map Feature.parse flags) in
  let native = Feature.(parse native) in
  let native_dynlink = Feature.(parse native_dynlink) in
  let comp =
    let doc = Arg.info
        ~doc:"Additional options passed to both the native and bytecode the \
              compilers."
        ~docv:"OPTIONS" ["comp"] in
    Arg.(value & opt (some string) None & doc) in
  let link =
    let doc = Arg.info
        ~doc:"Additional options passed to both the native and bytecode the \
              linkers."
        ~docv:"OPTIONS"["link"] in
    Arg.(value & opt (some string) None & doc) in
  let pp =
    let doc = Arg.info
        ~doc:"Additional options passed to the pre-processor."
        ~docv:"OPTIONS" ["pp"] in
    Arg.(value & opt (some string) None & doc) in
  let build_dir =
    let doc = Arg.info
        ~doc:"The name of the directory where built artifacts are created."
        ~docv:"DIR" ["build_dir"] in
    Arg.(value & opt string "_build" & doc) in
  let nam =
    let doc = Arg.info
        ~doc:"The package name."
        ~docv:"NAME" ["name"] in
    Arg.(value & opt (some string) None & doc) in
  let version =
    let doc = Arg.info
        ~doc:"The package version."
        ~docv:"VERSION" ["version"] in
    Arg.(value & opt (some string) None & doc) in
  let includes =
    let doc = Arg.info
        ~doc:"A list of directories to includes when loading `configure.ml'."
        ~docv:"DIRECTORY" ["I"] in
    Arg.(value & opt_all string [] & doc) in
  let disable_auto_load =
    let doc = Arg.info
        ~doc:"Do not auto-load of $(b,`ocamlfind query tools`/tools.cma) when \
              loading `configure.ml'."
        ["disable-auto-load-tools"] in
    Arg.(value & flag & doc) in

  let list = function
    | None   -> []
    | Some l -> [l] in

  let create (_,native) (_,native_dynlink)
      features comp link pp includes disable_auto_load build_dir name version = {
    native = native;
    native_dynlink = native_dynlink;
    features;
    comp = list comp;
    bytcomp = [];
    natcomp = [];
    link = list link;
    bytlink = [];
    natlink = [];
    pp = list pp;
    includes; auto_load = not disable_auto_load;
    build_dir;
    name;
    version;
  } in
  Term.(mk create $
        native $ native_dynlink $ flags $ comp $ link $ pp $
        includes $ disable_auto_load $ build_dir $ nam $ version)

let name t = t.name

let version t = t.version

let includes t = t.includes

let auto_load t = t.auto_load

let parse ?doc ?man name flags =
  let doc = match doc with
    | None   -> "helpers to manage and configure OCaml projects."
    | Some d -> d in
  let man =
    `S "DESCRIPTION"
    :: match man with
    | Some m -> List.map (fun p -> `P p) m
    | None   ->
      [`P "$(tname) is part of OCaml-tools, a collection of tools to \
           manage and configure OCaml projects."]
  in
  let info = Term.info name
      ~version:"0.1"
      ~sdocs:global_option_section
      ~doc
      ~man in
  match Term.eval (term flags, info) with
  | `Ok conf -> conf
  | `Version -> exit 0
  | `Help    -> exit 0
  | `Error _ -> exit 1
