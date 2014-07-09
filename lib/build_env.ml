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

open Cmdliner

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Thomas Gazagnaire <thomas@gazagnaire.org>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/assemblage/issues.";
]

type global = {
  mutable verbose: bool;
}

let global = {
  verbose = false;
}

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
  features: (Feature.t * bool) list;
  flags: Flags.t;
  includes: string list;
  auto_load: bool;
  build_dir: string;
}

let create
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
    () =
  let flags = Flags.create
      ~comp_byte:  (comp @ bytcomp)
      ~comp_native:(comp @ natcomp)
      ~link_byte:  (link @ bytlink)
      ~link_native:(link @ natlink)
      ~pp_byte:    pp
      ~pp_native:  pp
      () in
  { features; flags; build_dir; auto_load; includes }

let build_dir t = t.build_dir
let features t = t.features

let flags t = t.flags

let default = {
  features = [];
  flags = Flags.empty;
  auto_load = true;
  includes = [];
  build_dir = "_build";
}

let enable t flags =
  List.for_all (fun f ->
      try List.assoc f t.features
      with Not_found -> false
    ) flags

let term_of_list list =
  let aux acc h = Term.(pure (fun f t -> f :: t) $ h $ acc) in
  List.fold_left aux (Term.pure []) list

let term features: t Cmdliner.Term.t =
  let features = Feature.Set.elements features in
  let features = term_of_list (List.map Feature.parse features) in
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

  let create
      features comp link pp includes disable_auto_load build_dir =
    let link = list link in
    let comp = list comp in
    let pp = list pp in
    let auto_load = not disable_auto_load in
    create ~features ~comp ~link ~pp ~includes ~auto_load ~build_dir () in
  Term.(mk create $ features $ comp $ link $ pp $ includes $ disable_auto_load
        $ build_dir)

let includes t = t.includes

let auto_load t = t.auto_load

let parse ?doc ?man name features =
  let doc = match doc with
    | None   -> "helpers to manage and configure OCaml projects."
    | Some d -> d in
  let man =
    `S "DESCRIPTION"
    :: match man with
    | Some m -> List.map (fun p -> `P p) m
    | None   ->
      [`P "$(tname) is part of Assemblage, a collection of tools to \
           manage and configure OCaml projects."]
      @ help_sections
  in
  let info = Term.info name
      ~version:"0.1"
      ~sdocs:global_option_section
      ~doc
      ~man in
  match Term.eval (term features, info) with
  | `Ok conf -> conf
  | `Version -> exit 0
  | `Help    -> exit 0
  | `Error _ -> exit 1