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

(* Metadata *)

type kind = [ `OCaml | `OCaml_toplevel | `C ]

let pp_kind ppf k = As_fmt.pp_str ppf begin match k with
  | `OCaml -> "OCaml" | `OCaml_toplevel -> "OCaml_toplevel" | `C -> "C"
  end

type meta = { kind : kind; native : bool; byte : bool; js : bool }

let inj, proj = As_part.(meta_key meta_deps_none)
let get_meta p = As_part.get_meta proj p
let meta ?byte ?native ?js kind =
  let def_byte, def_nat, def_js = match kind with
  | `OCaml -> true, true, false
  | `OCaml_toplevel -> true, false, false
  | `C -> false, true, false
  in
  let byte = match byte with None -> def_byte | Some b -> b in
  let native = match native with None -> def_nat | Some b -> b in
  let js = match js with None -> def_js | Some b -> b in
  inj { kind; byte; native; js }

let kind p = (get_meta p).kind
let byte p = (get_meta p).byte
let native p = (get_meta p).native
let js p = (get_meta p).js

let is_kind k p = match As_part.coerce_if `Bin p with
| None -> None
| Some p as r -> if kind p = k then r else None

let c = is_kind `C
let ocaml = is_kind `OCaml
let ocaml_toplevel = is_kind `OCaml_toplevel

(* Check *)

let check p =
  let bin = As_part.coerce `Bin p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind bin);
  true

(* Actions *)

let actions p =
  let bin = As_part.coerce `Bin p in
  As_log.warn "%a part actions are TODO" As_part.pp_kind (As_part.kind bin);
  []

(* Part *)

let v ?usage ?cond ?args ?byte ?native ?js name kind needs =
  let meta = meta ?byte ?native ?js kind in
  As_part.v_kind ?usage ?cond ?args ~meta ~needs ~actions ~check name `Bin

let of_base ?byte ?native ?js kind p =
  let meta = meta ?byte ?native ?js kind in
  As_part.with_kind_meta `Bin meta p
