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

let str = Printf.sprintf
open Asd_ocaml_incl

let init args = [] (* TODO *)
(*
  match As_args.get (`Pp `Byte) args with
  | [] -> Clflags.preprocessor := None;
  | pp ->
    let pp = String.concat " " pp in
    Clflags.preprocessor := Some (str "camlp4o %s" pp)
*)

let modules ~build_dir unit = [] (* TODO *)
(*
  let r = As_ocamlfind.resolver `Direct  ~build_dir () in
  let () = init (As_part.flags (`Unit unit) r) in
  let aux ext =
    let source =
      As_part.source_dir (`Unit unit) (ext :> As_action.file)
    in
    let build =
      As_part.source_dir (`Unit unit) (ext :> As_action.file)
    in
    let parse f = match ext with
    | `Ml -> modules_of_ml (Pparse.parse_implementation ~tool_name:""
                              Format.err_formatter f)
    | `Mli -> modules_of_mli (Pparse.parse_interface ~tool_name:""
                                Format.err_formatter f)
    in
    if Sys.file_exists source then parse source else
    if Sys.file_exists build then parse build else
    As_string.Set.empty
  in
  let set =
    if As_part.products  `Mli unit then aux `Mli
    else if As_part.Unit.has `Ml unit then aux `Ml
    else As_string.Set.empty in
    As_string.Set.elements set
*)
