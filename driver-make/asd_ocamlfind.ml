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

open Assemblage

let str = Printf.sprintf

let (|>) x f = f x
let conmap f l = List.concat (List.map f l)

type syntax = [ `Shell | `Makefile ]
type mode = [ `Static | `Dynamic of [ `Shell | `Makefile ] ]

let query_args ?predicates ?format ?(uniq = false) ?(recursive = false) pkgs =
  let predicates = match predicates with
  | None   -> ""
  | Some p ->
      let has pkg = List.mem pkg pkgs in
      let p = if has "threads.posix" then "mt" :: "mt_posix" ::p else p in
      let p = if has "threads.vm" then "mt" :: "mt_vm" :: p else p in
      str "-predicates %s " (String.concat "," p)
  in
  let format = match format with
  | None   -> ""
   | Some f -> str "-format \"%s\" " f
  in
  let recursive = if recursive then "-r " else "" in
  let uniq = if uniq then [" | uniq"] else [] in
  ["ocamlfind"; "query"; recursive; predicates; format ] @ pkgs @ uniq

let query_static =
  let cache = Hashtbl.create 124 in
  let run cmd = try Hashtbl.find cache cmd with
  | Not_found ->
      let r = Asd_shell.exec_output "%s" cmd in
      Hashtbl.add cache cmd r;
      r
  in
  fun ?predicates ?format ?(uniq=false) ?(recursive=false) pkgs ->
    let cmd = query_args ?predicates ?format ~uniq ~recursive pkgs in
    run (String.concat " " cmd)

let query_makefile ?predicates ?format ?uniq:_ ?(recursive=false) pkgs =
  [ "$(shell" ] @ query_args ?predicates ?format ~recursive:true pkgs @ [ ")" ]

let query ~mode = match mode with
  | `Static -> query_static
  | `Dynamic `Shell -> query_args
  | `Dynamic `Makefile -> query_makefile

let includes ~mode ~recursive ~predicates pkgs =
  query ~mode ~recursive ~predicates ~format:"-I %d" ~uniq:true pkgs

let comp_byte ~mode pkgs =
  includes ~mode ~recursive:true ~predicates:["byte"] pkgs

let comp_native ~mode pkgs =
  includes ~mode ~recursive:true ~predicates:["native"] pkgs

let link_byte ~mode pkgs =
  includes ~mode ~recursive:true ~predicates:["byte"] pkgs @
  query ~mode ~predicates:["byte"] ~format:"%d/%a" ~recursive:true pkgs

let link_native ~mode pkgs =
  includes ~mode ~recursive:true ~predicates:["native"] pkgs @
  query ~mode ~predicates:["native"] ~format:"%d/%a" ~recursive:true pkgs

let pp_byte ~mode pkgs =
  (* https://github.com/samoht/assemblage/pull/119 *)
  link_byte ~mode pkgs @
  query ~mode ~predicates:["syntax"; "preprocessor"] ~recursive:true
    ~format:"-I %d %a" pkgs

let pkgs_args ~mode = function
| [] -> Args.empty
| pkgs ->
    Args.concat
      [ Args.create (`Pp `Byte) (pp_byte ~mode pkgs);
        Args.create (`Pp `Native) (pp_byte ~mode pkgs);
        Args.create (`Compile `Byte) (comp_byte ~mode pkgs);
        Args.create (`Compile `Native) (comp_native ~mode pkgs);
        Args.create (`Link `Byte) (link_byte ~mode pkgs);
        Args.create (`Link `Native) (link_native ~mode pkgs) ]

let available () = Asd_shell.has_cmd "ocamlfind"

module META = struct
  type t = string

  let of_project ~version t =
    let libs = Part.(keep_map Lib.ocaml) (Project.parts t) in
    let buf = Buffer.create 1024 in
    let one lib =
      let requires =
        conmap
          Part.deps ([lib])
        |> Part.(keep_map Pkg.ocaml)
        |> List.map Part.name
        |> String.concat " "
      in
      let name = Part.name lib in
      Printf.bprintf buf "version  = \"%s\"\n" version;
      Printf.bprintf buf "requires = \"%s\"\n" requires;
      Printf.bprintf buf "archive(byte) = \"%s.cma\"\n" name;
      Printf.bprintf buf "archive(byte, plugin) = \"%s.cma\"\n" name;
      Printf.bprintf buf "archive(native) = \"%s.cmxa\"\n" name;
      Printf.bprintf buf "archive(native, plugin) = \"%s.cmxs\"\n" name;
      Printf.bprintf buf "exist_if = \"%s.cma\"\n" name
    in
    List.iteri (fun i lib ->
        if i = 0 then one lib
        else (
          Printf.bprintf buf "package \"%s\" (" (Part.name lib);
          one lib;
          Printf.bprintf buf ")\n"
        )
      ) libs;
    Buffer.contents buf

  let write ?dir t =
    let file = match dir with
    | None   -> "META"
    | Some d -> Filename.concat d "META"
    in
    match t with
    | "" ->
        Log.show "%a@ skip@ %s" Fmt.(pp_styled `Yellow pp_rarrow) () file
    | _  ->
        Log.show "%a@ write@ %s" Fmt.(pp_styled `Green pp_rarrow) () file;
        let oc = open_out file in
        output_string oc t;
        close_out oc
end
