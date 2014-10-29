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

(** Command contexts

    For documentation see {!Assemblage.Ctx}. *)

(** {1 Context elements} *)

type build_phase =
  [ `Prepare | `Gen | `Dep | `Pp | `Compile | `Archive | `Link | `Doc ]

type language = [ `OCaml | `C | `Js ]
type ocaml_product = [ `Intf | `Byte | `Native | `Js ]
type archive_product = [ `Shared ]
type command = [ `Cmd of string As_conf.key ]
type tag = [ `Tag of string ]
type elt =
  [ build_phase | language | ocaml_product | archive_product | command | tag ]

val pp_elt : Format.formatter -> elt -> unit

(** {1 Contexts} *)

type t
val v : elt list -> t
include Set.S with type elt := elt
               and type t := t

val pp : Format.formatter -> t -> unit
val matches : t -> t -> bool
