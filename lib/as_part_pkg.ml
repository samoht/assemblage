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

(* Metadata *)

type other = [ `Other of string * As_args.t ]
type kind = [ `OCaml of [`OCamlfind | other ]
              | `C of [ `Pkg_config | other ]]

let pp_kind ppf = function
| `OCaml `OCamlfind -> As_fmt.pp_str ppf "OCaml (ocamlfind)"
| `OCaml (`Other (n, _)) -> As_fmt.pp ppf "OCaml (%s)" n
| `C `Pkg_config -> As_fmt.pp_str ppf "C (pkg-config)"
| `C (`Other (n, _)) -> As_fmt.pp ppf "C (%s)" n

type meta = { kind : kind; lookup : As_args.t; }
let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta kind lookup = inj { kind; lookup }

let kind p = (get_meta p).kind
let lookup p = (get_meta p).lookup

let is_kind k p = match As_part.coerce_if `Pkg p with
| None -> None
| Some p as r ->
    match kind p with
    | `OCaml _ when k = `OCaml -> r
    | `C _ when k = `C -> r
    | _ -> None

let ocaml = is_kind `OCaml
let c = is_kind `C

let lookup_args = function
| `OCaml (`Other (_, args)) -> args
| `C (`Other (_, args)) -> args
| `OCaml `OCamlfind -> As_args.empty (* TODO *)
| `C `Pkg_config -> As_args.empty (* TODO *)

(* Checks *)

let check p =
  let pkg = As_part.coerce `Pkg p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind pkg);
  true

(* Packages *)

let v ?usage ?cond ?args name kind =
  let lookup = lookup_args kind in
  let meta = meta kind lookup in
  As_part.v_kind ?usage ?cond ?args ~meta ~check name `Pkg

let of_base kind p =
  let lookup = lookup_args kind in
  let meta = meta kind lookup in
  As_part.with_kind_meta `Pkg meta p
