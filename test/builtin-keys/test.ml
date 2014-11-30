(*
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

(* Uses all the builtin keys so that we can check their documentation,
   defaults and parsing. *)

open Assemblage
open Assemblage.Private
open Assemblage_driver

let test_keys =
  let add_key k acc = Conf.Key.(Set.add (hide_type k) acc) in
  Conf.Key.Set.empty
  |> add_key Conf.project_version
  (**)
  |> add_key Conf.uname
  |> add_key Conf.host_os
  |> add_key Conf.host_arch
  |> add_key Conf.host_word_size
  |> add_key Conf.target_os
  |> add_key Conf.target_arch
  |> add_key Conf.target_word_size
  (**)
  |> add_key Conf.root_dir
  |> add_key Conf.build_dir
  (**)
  |> add_key Conf.debug
  |> add_key Conf.profile
  |> add_key Conf.warn_error
  |> add_key Conf.test
  |> add_key Conf.doc
  |> add_key Conf.jobs
  (**)
  |> add_key Conf.ocaml_native_tools
  |> add_key Conf.ocaml_version
  |> add_key Conf.ocaml_byte
  |> add_key Conf.ocaml_native
  |> add_key Conf.ocaml_native_dynlink
  |> add_key Conf.ocaml_build_ast
  |> add_key Conf.ocaml_js
  |> add_key Conf.ocaml_annot
  |> add_key Conf.ocaml_dumpast
  |> add_key Conf.ocamlc
  |> add_key Conf.ocamlopt
  |> add_key Conf.js_of_ocaml
  |> add_key Conf.ocamldep
  |> add_key Conf.ocamlmklib
  |> add_key Conf.ocamldoc
  |> add_key Conf.ocamllex
  |> add_key Conf.ocamlyacc
  |> add_key Conf.ocaml
  |> add_key Conf.ocamlrun
  |> add_key Conf.ocamldebug
  |> add_key Conf.ocamlprof
  |> add_key Conf.ocamlfind
  |> add_key Conf.opam
  |> add_key Conf.opam_installer
  |> add_key Conf.opam_admin
  (**)
  |> add_key Conf.c_dynlink
  |> add_key Conf.c_js
  |> add_key Conf.cc
  |> add_key Conf.pkg_config
  (**)
  |> add_key Conf.ln
  |> add_key Conf.cp
  |> add_key Conf.mv
  |> add_key Conf.cd
  |> add_key Conf.rm
  |> add_key Conf.rmdir
  |> add_key Conf.mkdir
  |> add_key Conf.cat
  |> add_key Conf.make

open Cmdliner

let main () =
  let conf = Driver.Conf_spec test_keys in
  let man = Driver.Conf_spec.man test_keys in
  let main conf = Format.fprintf "@[%a@]@." Conf.pp conf in
  let main = Term.(pure main $ conf) in
  let exec_name = Filename.basename Sys.argv.(0) in
  let info =
    let doc = "inspect all built-in configuration keys" in
    Term.info exec_name ~doc ~man
  in
  match Term.eval (main, info) with `Error _ -> exit 1 | _ -> exit 0

let () = main ()
