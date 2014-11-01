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

let str = Printf.sprintf

(* Variables *)

type var =
  { name : string;
    op : string;
    def : string list; }

let ( === ) name def = `Var { name; op = "="; def }
let ( =:= ) name def = `Var { name; op = ":="; def }
let ( =::= ) name def = `Var { name; op = "::="; def }
let ( =+= ) name def = `Var { name; op = "+="; def }
let ( =?= ) name def = `Var { name; op = "?="; def }

module Infix = struct
  let ( === ) = ( === )
  let ( =:= ) = ( =:= )
  let ( =::= ) = ( =::= )
  let ( =+= ) = ( =+= )
  let ( =?= ) = ( =?= )
end

(* Rules *)

type rule =
  { ext : bool;
    targets : string list;
    prereqs : string list;
    order_only_prereqs : string list;
    recipe : string list list; }

let rule ?(ext = false) ?(order_only_prereqs = []) ~targets ~prereqs ~recipe
    () =
  `Rule { ext; targets; prereqs; order_only_prereqs; recipe; }

(* Makefiles *)

type statement =
  [ `Var of var
  | `Rule of rule ]

type t = [ statement | `Comment of string | `Blank ] list

let buf_add_strings ?(max = 76) ?(nl = "\\\n") ?(indent = "    ") b count ss =
  let indent_len = String.length indent in
  let rec loop count = function
  | [] -> ()
  | d :: ds as defs ->
      let new_len = String.length d + count + 1 in
      if new_len > max && count > indent_len
      then (Buffer.add_string b (str "%s%s" nl indent); loop indent_len defs)
      else begin
        Buffer.add_string b d;
        if ds <> [] then (Buffer.add_char b ' '; loop new_len ds)
      end
  in
  loop count ss

let buf_add_var b { name; op; def } =
  Buffer.add_string b name;
  Buffer.add_char b ' ';
  Buffer.add_string b op;
  Buffer.add_char b ' ';
  buf_add_strings b String.(length name + length op + 2) def;
  Buffer.add_char b '\n';
  ()

let buf_add_cmd b cmd =
  Buffer.add_string b "\t";
  buf_add_strings ~indent:"\t  " b 4 cmd;
  Buffer.add_char b '\n';
  ()

let buf_add_rule b { ext; targets; prereqs; order_only_prereqs; recipe } =
  let has_oo = order_only_prereqs <> [] in
  let oo = if has_oo then "|" :: order_only_prereqs else [] in
  buf_add_strings ~indent:"" b 0 targets;
  Buffer.add_string b (if ext then "::" else ":");
  Buffer.add_string b " \\\n    ";
  buf_add_strings b 4 (prereqs @ oo);
  Buffer.add_char b '\n';
  List.iter (buf_add_cmd b) recipe;
  Buffer.add_char b '\n';
  ()

let buf_add_comment b c =
  let indent = "# " in
  Buffer.add_string b indent;
  buf_add_strings ~nl:"\n" ~indent b 2 (As_string.split ~sep:" " c);
  Buffer.add_char b '\n';
  ()

let to_string mk =
  let b = Buffer.create 8192 in
  let add = function
  | `Blank -> Buffer.add_char b '\n'
  | `Var v -> buf_add_var b v
  | `Rule r -> buf_add_rule b r
  | `Comment c -> buf_add_comment b c
  in
  List.iter add mk;
  Buffer.contents b
