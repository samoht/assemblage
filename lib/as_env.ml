(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Daniel C. Bünzli
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

type t =
  { ocamlc : string;
    ocamlopt : string;
    ocamldep : string;
    ocamlmklib : string;
    ocamldoc : string;
    ocaml_pp : string option;
    js_of_ocaml : string;
    mkdir : string;
    ln : string;
    build_dir : As_path.rel;
    root_dir : As_path.t;
    ocamlfind_pkgs : string list -> As_args.t;
    pkg_config : string list -> As_args.t; }

let create
    ?(ocamlc = "ocamlc")
    ?(ocamlopt = "ocamlopt")
    ?(ocamldep = "ocamldep")
    ?(ocamlmklib = "ocamlmklib")
    ?(ocamldoc = "ocamldoc")
    ?(ocaml_pp = Some "ocaml-dumpast")
    ?(ln = "ln -sf")
    ?(mkdir = "mkdir -p")
    ?(js_of_ocaml = "js_of_ocaml")
    ?(build_dir = As_path.dir "_build")
    ?root_dir
    ?(ocamlfind_pkgs = fun _ -> As_args.empty)
    ?(pkg_config = fun _ -> As_args.empty) () =
  let root_dir = match root_dir with
  | Some d -> d
  | None -> As_path.of_string (Sys.getcwd ())
  in
  { ocamlc; ocamlopt; ocamlmklib; ocaml_pp; js_of_ocaml; ocamldoc;
    ocamldep; ln; mkdir; build_dir; root_dir; ocamlfind_pkgs; pkg_config }

(* Directories *)

let build_dir t = t.build_dir
let root_dir t = t.root_dir
let push_build_dir t b =
  { t with build_dir = As_path.(as_rel (t.build_dir // b)) }

(* Program binaries *)

let ocamlc t = t.ocamlc
let ocamlopt t = t.ocamlopt
let ocamldep t = t.ocamldep
let ocamlmklib t = t.ocamlmklib
let ocamldoc t = t.ocamldoc
let ocaml_pp t = t.ocaml_pp
let js_of_ocaml t = t.js_of_ocaml
let ln t = t.ln
let mkdir t = t.mkdir

(* Package queries *)

let ocamlfind_pkgs t = t.ocamlfind_pkgs
let pkg_config t = t.pkg_config
