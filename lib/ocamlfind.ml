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

type mode = [`Direct|`Indirect|`Makefile]

let query_indirect ?predicates ?format ?(uniq=false) ?(recursive=false) packages =
  let predicates = match predicates with
    | None   -> ""
    | Some p -> sprintf "-predicates %s " (String.concat "," p) in
  let format = match format with
    | None   -> ""
    | Some f -> sprintf "-format \"%s\" " f in
  let recursive = match recursive with
    | true   -> "-r "
    | false  -> "" in
  let uniq = match uniq with
    | true   -> " | uniq"
    | false  -> "" in
  let packages = String.concat " " packages in
  let args = String.concat "" [
      recursive; predicates; format; recursive; packages; uniq
    ] in
  sprintf "ocamlfind query %s" args

let cache = Hashtbl.create 124

let query_direct ?predicates ?format ?(uniq=false) ?(recursive=false) packages =
  let cmd = query_indirect ?predicates ?format ~uniq ~recursive packages in
  try Hashtbl.find cache cmd
  with Not_found ->
    let r = String.concat " " (Shell.exec_output "%s" cmd) in
    Hashtbl.add cache cmd r;
    r

let query_makefile ?predicates ?format ?(uniq=false) ?(recursive=false) packages =
  sprintf "$(shell %s)"
    (query_indirect ?predicates ?format ~uniq ~recursive packages)

let query ~mode = match mode with
  | `Direct   -> query_direct
  | `Indirect -> query_indirect
  | `Makefile -> query_makefile

let pp_byte ~mode names =
  [query ~mode
    ~predicates:["syntax";"preprocessor"]
    ~recursive:true
    ~format:"%d/%a"
    names]

let pp_native ~mode names =
  [query ~mode
    ~predicates:["syntax";"preprocessor";"native"]
    ~recursive:true
    ~format:"%d/%a"
    names]

let comp_byte ~mode names =
  [query ~mode
    ~predicates:["byte"]
    ~format:"-I %d"
    ~recursive:true
    ~uniq:true
    names]

let comp_native ~mode names =
  [query ~mode
    ~predicates:["native"]
    ~format:"-I %d"
    ~recursive:true
    ~uniq:true
    names]

let link_byte ~mode names =
  [query ~mode
    ~predicates:["byte"]
    ~format:"%d/%a"
    ~recursive:true
    names]

let link_native ~mode names =
  [query ~mode
    ~predicates:["native"]
    ~format:"%d/%a"
    ~recursive:true
    names]

let pkgs ~mode names =
  let pp_byte     = pp_byte     ~mode names in
  let pp_native   = pp_native   ~mode names in
  let comp_byte   = comp_byte   ~mode names in
  let comp_native = comp_native ~mode names in
  let link_byte   = link_byte   ~mode names in
  let link_native = link_native ~mode names in
  Flags.create
    ~pp_byte ~pp_native
    ~comp_byte ~comp_native
    ~link_byte ~link_native ()

let resolver mode build_dir =
  Resolver.create ~build_dir ~pkgs:(pkgs ~mode)

module META = struct

  type t = string

  let of_project t =
    let libs = Dep.(filter lib @@ Project.contents t) in
    let version = Project.version t in
    let buf = Buffer.create 1024 in
    let one lib =
      let requires = Lib.deps lib |> Dep.(filter pkg) |> String.concat " " in
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
