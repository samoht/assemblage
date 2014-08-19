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
module StringSet = Set.Make (String)

let (/) = Filename.concat
let (|>) x f = f x
let conmap f l = List.concat (List.map f l)

type mode = [`Direct|`Indirect|`Makefile]

let query_indirect ?predicates ?format ?(uniq=false) ?(recursive=false)
    packages =
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
  let packages = String.concat " \\\n            " packages in
  let args = String.concat "" [
      recursive; predicates; format; recursive; packages; uniq
    ] in
  [sprintf "ocamlfind query %s" args]

let cache = Hashtbl.create 124

let run cmd =
  try Hashtbl.find cache cmd
  with Not_found ->
    let r = As_shell.exec_output "%s" cmd in
    Hashtbl.add cache cmd r;
    r

let query_direct ?predicates ?format ?(uniq=false) ?(recursive=false) packages =
  let cmd = query_indirect ?predicates ?format ~uniq ~recursive packages in
  run (String.concat " " cmd)

let query_makefile ?predicates ?format ?uniq:_ ?(recursive=false) packages =
  [ sprintf "$(shell %s)"
      (String.concat " "
         (query_indirect ?predicates ?format ~recursive:true packages)) ]

let query ~mode = match mode with
  | `Direct   -> query_direct
  | `Indirect -> query_indirect
  | `Makefile -> query_makefile

let pp_byte ~mode names =
  query ~mode
    ~predicates:["syntax";"preprocessor"]
    ~recursive:true
    ~format:"%d/%a"
    names

(*
let pp_native ~mode names =
  query ~mode
    ~predicates:["syntax";"preprocessor";"native"]
    ~recursive:true
    ~format:"%d/%a"
    names
*)

let comp_byte ~mode names =
  query ~mode
    ~predicates:["byte"]
    ~format:"-I %d"
    ~recursive:true
    ~uniq:true
    names

let comp_native ~mode names =
  query ~mode
    ~predicates:["native"]
    ~format:"-I %d"
    ~recursive:true
    ~uniq:true
    names

let link_byte ~mode names =
  query ~mode
    ~predicates:["byte"]
    ~format:"%d/%a"
    ~recursive:true
    names

let link_native ~mode names =
  query ~mode
    ~predicates:["native"]
    ~format:"%d/%a"
    ~recursive:true
    names

let pkgs pkgs_defs ~mode names =
  let cache phase result =
    let pkgs = String.concat "_" names in
    let var = sprintf "pkgs_%s_%s" pkgs (As_flags.string_of_phase phase) in
    Hashtbl.replace pkgs_defs var (List.hd result);
    As_flags.v phase [(sprintf "$(%s)" var)]
  in
  let open As_flags in
  cache (`Pp `Byte) (pp_byte ~mode names) @@@
  cache (`Pp `Native) (pp_byte ~mode names) @@@
  cache (`Compile `Byte) (comp_byte ~mode names) @@@
  cache (`Compile `Native) (comp_native ~mode names) @@@
  cache (`Link `Byte) (link_byte ~mode names) @@@
  cache (`Link `Native) (link_native ~mode names)

let resolver mode =
  if As_shell.try_exec "ocamlfind list" then
    let pkgs_defs = Hashtbl.create 255 in
    As_resolver.create ~pkgs:(pkgs pkgs_defs ~mode) ~pkgs_defs
  else
  As_shell.fatal_error 1 "ocamlfind is not installed on your system, stopping."

module META = struct

  type t = string

  let of_project t =
    let libs =
      As_component.(filter_map lib_ocaml) (As_project.components t)
    in
    let version = As_project.version t in
    let buf = Buffer.create 1024 in
    let one lib =
      let c = `Lib lib in
      let requires =
        conmap
          As_component.deps (c :: As_component.contents c)
        |> As_component.closure
        |> As_component.(filter_map pkg_ocaml)
        |> List.map As_component.Pkg.name
        |> String.concat " " in
      let name = As_component.name (`Lib lib) in
      bprintf buf "version  = \"%s\"\n" version;
      bprintf buf "requires = \"%s\"\n" requires;
      bprintf buf "archive(byte) = \"%s.cma\"\n" name;
      bprintf buf "archive(byte, plugin) = \"%s.cma\"\n" name;
      bprintf buf "archive(native) = \"%s.cmxa\"\n" name;
      bprintf buf "archive(native, plugin) = \"%s.cmxs\"\n" name;
      bprintf buf "exist_if = \"%s.cma\"\n" name in
    List.iteri (fun i lib ->
        if i = 0 then one lib
        else (
          bprintf buf "package \"%s\" (" (As_component.name (`Lib lib));
          one lib;
          bprintf buf ")\n"
        )
      ) libs;
    Buffer.contents buf

  let write ?dir t =
    let file = match dir with
      | None   -> "META"
      | Some d -> d / "META" in
    match t with
    | "" -> printf "%s skip  %s\n" (As_shell.color `Yellow "==>") file
    | _  ->
        printf "%s write %s\n" (As_shell.color `Green "==>") file;
        let oc = open_out file in
        output_string oc t;
        close_out oc

end
