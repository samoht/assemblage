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

type kind = [ `OCaml | `OCaml_toplevel | `C ]
type meta =
  { kind : kind;
    native : bool;
    byte : bool;
    js : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?(byte = true) ?(native = true) ?(js = true) kind =
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

(* Actions *)

let ocaml_actions units p = []

let actions units p = []
(*
    let b = coerce `Bin p in
    let build_dir = As_path.dir (kind_to_string `Bin ^ "-" ^ (name b)) in
    let env = As_env.push_build_dir env build_dir in
    let units_actions = List.(flatten (map actions units)) in
    units_actions @ ocaml_actions units p
*)

(* Create *)

let create ?cond ?(args = As_args.empty) ?deps:(ds = []) ?byte ?native
    ?js name kind units =
  let meta = meta ?byte ?native ?js kind in
  let deps = ds (* @ List.flatten (List.map deps units) *) in
  let units = (* List.map (add_deps_args deps args) *) units in
  let args _ = args in
  let actions = actions units in
  As_part.create ?cond ~args ~deps ~actions name `Bin meta

let of_base ?byte ?native ?js kind p =
  let meta = meta ?byte ?native ?js kind in
  { p with As_part.kind = `Bin; meta }

(* As build commands *)

(*
  let cmd ?(args = As_args.empty) ?kind bin args' =
    args, fun args -> "TODO EXEC NAME" :: (args' args)
*)
