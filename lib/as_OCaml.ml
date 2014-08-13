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
open As_OCaml_incl

module StringSet = Set.Make (String)

let init flags =
  match As_flags.get (`Pp `Byte) flags with
  | [] -> Clflags.preprocessor := None;
  | pp ->
    let pp = String.concat " " pp in
    Clflags.preprocessor := Some (sprintf "camlp4o %s" pp)

let modules ~build_dir unit =
  let r = As_ocamlfind.resolver `Direct  ~build_dir () in
  let () = init (As_project.Unit.flags unit r) in
  let aux ext =
    let source = As_project.Component.source (`Unit unit) (ext:>As_action.file) in
    let build = As_project.Component.source (`Unit unit) (ext:>As_action.file) in
    let parse f = match ext with
    | `Ml -> modules_of_ml (Pparse.parse_implementation ~tool_name:""
                              Format.err_formatter f)
    | `Mli -> modules_of_mli (Pparse.parse_interface ~tool_name:""
                                Format.err_formatter f)
    in
    if Sys.file_exists source then parse source else
    if Sys.file_exists build then parse build else
    StringSet.empty
  in
  let set =
    if As_project.Unit.has `Mli unit then aux `Mli
    else if As_project.Unit.has `Ml unit then aux `Ml
    else StringSet.empty in
  StringSet.elements set
