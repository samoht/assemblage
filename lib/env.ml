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

module Flag = struct

  type t = {
    name: string;
    default: bool;
    doc: string;
  }

  let name t = t.name

  let doc t = t.doc

  let default t = t.default

  let create ~doc ~default name = { name; default; doc }

  let parse t =
    let enable =
      let d = Arg.info ~doc:(sprintf "Enable %s" t.doc)
          ["enable-" ^ t.name] in
      Arg.(value & flag & d) in
    let disable =
      let d = Arg.info ~doc:(sprintf "Disable %s" t.doc)
          ["disable-" ^ t.name] in
      Arg.(value & flag & d) in
    let create enable disable =
      let v = match enable, disable with
        | true , false -> true
        | false, true  -> false
        | false, false -> t.default
        | true , true  -> failwith "Invalid flag" in
      (t, v) in
    Term.(mk create $ enable $ disable)

  let native =
    create ~doc:"native code compilation." ~default:true "native"

  let native_dynlink =
    create ~doc:"native plugins for native code." ~default:true "native-dynlink"

end

type t = {
  native: bool;
  native_dynlink: bool;
  flags: (Flag.t * bool) list;
  comp: string list;
  bytcomp: string list;
  natcomp: string list;
  link: string list;
  bytlink: string list;
  natlink: string list;
  p4o: string list;
  destdir: string;
  name: string option;
  version: string option;
}

let create
    ?(native=true)
    ?(native_dynlink=true)
    ?(flags=[])
    ?(comp=[])
    ?(bytcomp=[])
    ?(natcomp=[])
    ?(link=[])
    ?(bytlink=[])
    ?(natlink=[])
    ?(p4o=[])
    ?(destdir="_build")
    ?name ?version
    () =
  { native; native_dynlink; flags; comp; bytcomp; natcomp;
    link; bytlink; natlink; p4o; destdir; name; version }

let destdir t = t.destdir
let native t = t.native
let native_dynlink t = t.native && t.native_dynlink
let flags t = t.flags

let comp t = t.comp
let bytcomp t = t.bytcomp @ t.comp
let natcomp t = t.natcomp @ t.comp

let link t = t.link
let bytlink t = t.bytlink @ t.link
let natlink t = t.natlink @ t.link

let p4o t = t.p4o

let default = {
  native = true;
  native_dynlink = true;
  flags = [];
  comp = [];
  bytcomp = [];
  natcomp = [];
  link = [];
  bytlink = [];
  natlink = [];
  p4o = [];
  destdir = "_build";
  name = None;
  version = None;
}

let enable t flags =
  List.for_all (fun f ->
      try List.assoc f t.flags
      with Not_found -> false
    ) flags

let term_of_list list =
  let aux acc h = Term.(pure (fun f t -> f :: t) $ h $ acc) in
  List.fold_left aux (Term.pure []) list

let parse flags =
  let flags = term_of_list (List.map Flag.parse flags) in
  let native = Flag.(parse native) in
  let native_dynlink = Flag.(parse native_dynlink) in
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
  let p4o =
    let doc = Arg.info
        ~doc:"Additional options passed to the camlp4o pre-processor."
        ~docv:"OPTIONS" ["p4o"] in
    Arg.(value & opt (some string) None & doc) in
  let destdir =
    let doc = Arg.info
        ~doc:"The name of the directory where built artifacts are created."
        ~docv:"DIR" ["destdir"] in
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

  let list = function
    | None   -> []
    | Some l -> [l] in

  let create (_,native) (_,native_dynlink) flags comp link p4o destdir name version = {
    native = native;
    native_dynlink = native_dynlink;
    flags;
    comp = list comp;
    bytcomp = [];
    natcomp = [];
    link = list link;
    bytlink = [];
    natlink = [];
    p4o = list p4o;
    destdir;
    name;
    version;
  } in
  Term.(mk create $
        native $ native_dynlink $ flags $ comp $ link $ p4o $ destdir $ nam $ version)

let name t = t.name

let version t = t.version

(*
let parse ?version flags =
  let doc = "opam-configure - helpers to manage and configure OCaml projects." in
  let man = [
    `S "DESCRIPTION";
    `P "opam-configure is part of OCaml-tools, a collection of tools to \
        manage and configure OCaml projects.";
  ] in
  let git_version = match Git.version () with
    | None   -> ""
    | Some v -> v in
  let version = match version with
    | None   -> git_version
    | Some v -> v ^ git_version in
  let info = Term.info "opam-configure"
      ~version
      ~sdocs:global_option_section
      ~doc
      ~man in
  match Term.eval ((Conf.parse flags), info) with
  | `Ok conf -> conf
  | `Version -> failwith "version"
  | `Help    -> failwith "help"
  | `Error _ -> exit 1
*)
