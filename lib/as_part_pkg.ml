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

type kind = [ `OCaml | `C ]
type meta = { kind : kind; }
let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta kind = inj { kind; }

let kind p = (get_meta p).kind
let is_kind k p = match As_part.coerce_if `Pkg p with
| None -> None
| Some p as r -> if kind p = k then r else None

let ocaml = is_kind `OCaml
let c = is_kind `C

type ocaml_lookup = [ `OCamlfind ]
type c_lookup = [ `Pkg_config ]
type spec = [ `C of c_lookup | `OCaml of ocaml_lookup ]

let ocamlfind_lookup name args _  = args
(*
    As_args.(As_env.ocamlfind_pkgs env [name] @@@ args)
*)

let pkg_config_lookup name args _ = args
(*
    As_args.(As_env.pkg_config env [name] @@@ args)
*)

let create ?cond ?(args = As_args.empty) name spec =
  let kind, args = match spec with
  | `OCaml `OCamlfind -> `OCaml, ocamlfind_lookup name args
  | `C `Pkg_config -> `C, pkg_config_lookup name args
  in
  let meta = meta kind in
  As_part.create ?cond ~args name `Pkg meta

let of_base kind p =
  let meta = meta kind in
  { p with As_part.kind = `Pkg; meta }
