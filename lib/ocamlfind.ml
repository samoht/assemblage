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

let (/) = Filename.concat

let pp_byte names l =
  let names = String.concat " " names in
  sprintf
    "$(shell ocamlfind query %s -r -predicates syntax,preprocessor \
     -format \"-I %%d %%a\")"
    names
  :: l

let pp_native names l =
  let names = String.concat " " names in
  sprintf
    "$(shell ocamlfind query %s -r -predicates syntax,preprocessor, native \
     -format \"-I %%d %%a\")"
    names
  :: l

let incl names l =
  let names = String.concat " "  (List.rev names) in
  sprintf
    "$(shell ocamlfind query %s -r -predicates byte -format \"-I %%d\")"
    names
  :: l

let comp_byte = incl

let comp_native = incl

let link_byte names l =
  let names = String.concat " "  (List.rev names) in
  sprintf
    "$(shell ocamlfind query %s -r -predicates byte -format \"-I %%d %%a\")"
    names
  :: l

let link_native names l =
  let names = String.concat " "  (List.rev names) in
  sprintf
    "$(shell ocamlfind query %s -r -predicates native -format \"-I %%d %%a\")"
    names
  :: l

let pkgs names =
  let pp_byte     = pp_byte names in
  let pp_native   = pp_native names in
  let comp_byte   = comp_byte names in
  let comp_native = comp_native names in
  let link_byte   = link_byte names in
  let link_native = link_native names in
  Flags.create
    ~pp_byte ~pp_native
    ~comp_byte ~comp_native
    ~link_byte ~link_native ()

let resolver buildir =
  Resolver.create ~buildir ~pkgs

module META = struct

  type t = string

  let of_project t =
    let libs = Project.libs t in
    let version = Project.version t in
    let buf = Buffer.create 1024 in
    let one lib =
      let requires = Lib.deps lib |> Dep.filter_pkgs |> String.concat " " in
      bprintf buf "version  = \"%s\"\n" version;
      bprintf buf "requires = \"%s\"\n" requires;
      bprintf buf "archive(byte) = \"%s.cma\"\n" (Lib.name lib);
      bprintf buf "archive(byte, plugin) = \"%s.cma\"\n" (Lib.name lib);
      bprintf buf "archive(native) = \"%s.cmxa\"\n" (Lib.name lib);
      bprintf buf "archive(native, plugin) = \"%s.cmxs\"\n" (Lib.name lib);
      bprintf buf "exist_if = \"%s.cma\"\n" (Lib.name lib) in
    List.iteri (fun i lib ->
        if i = 0 then one lib
        else (
          bprintf buf "package \"%s\" (" (Lib.name lib);
          one lib;
          bprintf buf ")\n"
        )
      ) libs;
    Buffer.contents buf

  let write ?dir t =
    let file = match dir with
      | None   -> "META"
      | Some d -> d / "META" in
    printf "\027[36m+ write %s\027[m\n" file;
    let oc = open_out file in
    output_string oc t;
    close_out oc

end
