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

type lookup = As_ctx.t -> string list
type kind =
  [ `OCamlfind
  | `Pkg_config
  | `Other of string * lookup As_conf.value ]

let pp_kind ppf = function
| `OCamlfind -> As_fmt.pp_str ppf "ocamlfind"
| `Pkg_config -> As_fmt.pp_str ppf "pkg-config"
| `Other (n, _) -> As_fmt.pp ppf "%s" n

type meta = { kind : kind; lookup : lookup As_conf.value; opt : bool }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?(opt = false) kind lookup = inj { kind; lookup; opt }

let kind p = (get_meta p).kind
let lookup p = (get_meta p).lookup
let opt p = (get_meta p).opt

let is_kind k p = match As_part.coerce_if `Pkg p with
| None -> None
| Some p as r ->
    match kind p with
    | `OCamlfind when k = `OCamlfind -> r
    | `Pkg_config when k = `Pkg_config -> r
    | `Other _ when k = `Other -> r
    | _ -> None

let ocamlfind = is_kind `OCamlfind
let pkg_config = is_kind `Pkg_config
let other = is_kind `Other

(* Checks *)

let check p =
  let pkg = As_part.coerce `Pkg p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind pkg);
  As_conf.true_

(* Packages *)

let lookup_value name = function
| `OCamlfind -> As_ocamlfind.lookup name
| `Pkg_config -> As_pkg_config.lookup name
| `Other (_, lookup) -> lookup

let v ?usage ?exists ?opt name kind =
  let lookup = lookup_value name kind in
  let meta = meta ?opt kind lookup in
  As_part.v_kind ?usage ?exists ~meta ~check name `Pkg

(* FIXME the following wont work if two packages depend
   on the same package we will get double linking. What
   strategy for uniq ? *)
let list_lookup ps =
  let add_pkg acc p = match As_part.coerce_if `Pkg p with
  | None -> acc
  | Some pkg ->
      let lookup = lookup p in
      let combine lookup acc ctx =
        List.rev_append (List.rev (lookup ctx)) (acc ctx)
      in
      As_conf.(const combine $ lookup $ acc)
  in
  List.fold_left add_pkg (As_conf.const (fun _ -> [])) (List.rev ps)
