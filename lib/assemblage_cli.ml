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

open Assemblage
open Assemblage.Private
open Cmdliner

module Cli = struct

  let uppercase = function None -> None | Some s -> Some (String.uppercase s)
  let value_converter_of_converter (parse, _) =
    let parse s = match parse s with
    | `Ok v -> `Ok (Conf.const v) | `Error _ as e -> e
    in
    let print = Fmt.nop (* config needed be accurate, use describe cmd *) in
    parse, print

  let term_of_conf c =
    let add (Conf.Key.V k) (names, acc) =
      if not (Conf.Key.public k) then (names, acc) else
      if As_string.Set.mem (Conf.Key.name k) names
      then (Log.warn "%a" Conf.pp_key_dup (Conf.Key.V k); (names, acc))
      else
      let names' = As_string.Set.add (Conf.Key.name k) names in
      let v = Conf.Key.default k in
      let c = value_converter_of_converter (Conf.Key.converter k) in
      let doc = Conf.Key.doc k in
      let docs = uppercase (Conf.Key.docs k) in
      let docv = Conf.Key.docv k in
      let i = Arg.info [Conf.Key.name k] ?doc ?docv ?docs in
      let opt = Arg.(value (opt c v & i)) in
      let acc' = Term.(pure Conf.set $ acc $ pure k $ opt) in
      (names', acc')
    in
    let acc = (As_string.Set.empty, Cmdliner.Term.pure c) in
    snd (Conf.Key.Set.fold add (Conf.domain c) acc)

  let builtin_sections =
    let open Conf in
    [ docs_project, doc_project;
      docs_build_properties, doc_build_properties;
      docs_build_directories, doc_build_directories;
      docs_ocaml_system, doc_ocaml_system;
      docs_c_system, doc_c_system;
      docs_machine_information, doc_machine_information;
      docs_system_utilities, doc_system_utilities; ]

  let man_of_conf c =
    let conf_sections =
      let add (Conf.Key.V k) acc = match Conf.Key.docs k with
      | None -> acc | Some sec -> String.Set.add sec acc
      in
      Conf.Key.Set.fold add (Conf.domain c) String.Set.empty
    in
    let add_section acc (title, doc) =
      if not (String.Set.mem title conf_sections) then acc else
      `P doc :: `S (String.uppercase title) :: acc
    in
    List.rev (List.fold_left add_section [] builtin_sections)

end
