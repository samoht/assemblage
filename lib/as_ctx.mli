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

open Bos

type tag = [ `Tag of string ]
type language = [ `OCaml | `C | `Js | `Lang of string ]
type build_phase =
  [ `Gen | `Dep | `Pp | `Compile | `Archive of [ `Static | `Shared ] | `Link
  | `Doc ]

type source = [ `Src of Path.ext ]
type target = [ `Target of [`Src | `Byte | `Native | `Js | `Other of string ]]
type cmd = [ `Cmd of string As_conf.key | `Cmd_static of string ]
type part_usage = [ `Build | `Dev | `Doc | `Other of string | `Outcome | `Test ]
type part_kind = [ `Base | `Bin | `Dir | `Doc | `Lib | `Pkg | `Run | `Unit ]
type part = [ `Part of [ part_usage | part_kind | `Name of string ]]
type elt = [ tag | language | build_phase | source | target | cmd | part ]

val pp_elt : Format.formatter -> elt -> unit
val pp_kind : Format.formatter -> part_kind -> unit
val pp_usage : Format.formatter -> part_usage -> unit

(** {1 Contexts} *)

type t
val v : elt list -> t
include Set.S with type elt := elt
               and type t := t

val pp : Format.formatter -> t -> unit
val matches : t -> t -> bool
