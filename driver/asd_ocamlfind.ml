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
  "ocamlfind", (["query"; recursive; predicates; format ] @ pkgs @ uniq)

let query_static =
  let cache = Hashtbl.create 124 in
  let run (cmd, args as l) = try Hashtbl.find cache l with
  | Not_found ->
      let r = Cmd.(on_error ~use:[] @@ read_lines cmd args) in
      Hashtbl.add cache l r;
      r
  in
  fun ?predicates ?format ?(uniq=false) ?(recursive=false) pkgs ->
    run (query_args ?predicates ?format ~uniq ~recursive pkgs)

let query_makefile ?predicates ?format ?uniq:_ ?(recursive=false) pkgs =
  let cmd, args = query_args ?predicates ?format ~recursive:true pkgs in
  [ "$(shell" ] @  (cmd :: args) @ [ ")" ]

let query ~mode = match mode with
  | `Static -> query_static
  | `Dynamic `Shell ->
      fun ?predicates ?format ?uniq ?recursive pkgs ->
        let cmd, args = query_args ?predicates ?format ?uniq ?recursive pkgs in
        cmd :: args
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
| pkgs -> Args.empty
(*
    Args.concat
      [ Args.create (`Pp `Byte) (pp_byte ~mode pkgs);
        Args.create (`Pp `Native) (pp_byte ~mode pkgs);
        Args.create (`Compile `Byte) (comp_byte ~mode pkgs);
        Args.create (`Compile `Native) (comp_native ~mode pkgs);
        Args.create (`Link `Byte) (link_byte ~mode pkgs);
        Args.create (`Link `Native) (link_native ~mode pkgs) ]
*)
