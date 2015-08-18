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

type kind = [ `OCamldoc ]

let pp_kind ppf k = Fmt.string ppf begin match k with
  | `OCamldoc -> "ocamldoc"
  end

type meta = { kind : kind }

let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta kind = inj { kind }
let kind p = (get_meta p).kind

let is_kind k p = match As_part.coerce_if `Doc p with
| None -> None
| Some p as r -> if kind p = k then r else None

let ocamldoc p = is_kind `OCamldoc p

(* Unit filters *)

let default p = match As_part_unit.kind p with
| `OCaml (_, `Hidden) -> false
| `OCaml _ -> true
| _ -> false

let dev p = match As_part_unit.kind p with `OCaml _ -> true | _ -> false

(* Check *)

let check p =
  let doc = As_part.coerce `Doc p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind doc);
  As_conf.true_

(* Actions *)

let actions p =
  let doc = As_part.coerce `Doc p in
  As_log.warn "%a part actions are TODO" As_part.pp_kind (As_part.kind doc);
  As_conf.const []

(* Doc *)

let v ?usage ?exists ?args ?keep name kind needs =
  let _keep = match keep with
  | Some k -> k
  | None -> if usage = Some `Dev then dev else default
  in
  let meta = meta kind in
  As_part.v_kind ?usage ?exists ?args ~meta ~needs ~actions ~check name `Doc
