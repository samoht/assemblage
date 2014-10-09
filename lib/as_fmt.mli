(*
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

(** Formatters.

    For documentation see {!Assemblage.Fmt}. *)

(** {1 Formatters} *)

type 'a formatter = Format.formatter -> 'a -> unit

val pp :
  Format.formatter -> ('a, Format.formatter, unit) Pervasives.format -> 'a

val rpp :
  ('a, Format.formatter, unit) Pervasives.format -> Format.formatter -> 'a

val nop : 'a formatter
val pp_cut : unit formatter
val pp_sp : unit formatter
val pp_str : string formatter
val pp_int : int formatter
val pp_bool : bool formatter
val pp_larrow : unit formatter
val pp_rarrow : unit formatter
val pp_opt : ?pp_none:unit formatter -> 'a formatter -> 'a option formatter
val pp_list : ?pp_sep:unit formatter -> 'a formatter -> 'a list formatter
val pp_text : string formatter
val pp_lines : string formatter

(** {1 Styled formatting} *)

type style_tags = [ `Ansi | `None ]
type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White ]

val style_tags : unit -> style_tags
val set_style_tags : style_tags -> unit
val pp_styled : style -> 'a formatter -> 'a formatter
val pp_styled_str : style -> string formatter
