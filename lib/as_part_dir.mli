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

(** Directory part.

    See {!Assemblage.Dir}. *)

(** {1 Metadata} *)

type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
            | `Etc | `Doc | `Stublibs | `Man | `Other of As_path.t ]

val pp_kind : Format.formatter -> kind -> unit

val kind : [< `Dir] As_part.t -> kind
val install : [< `Dir] As_part.t -> bool

(** {1 Product selectors} *)

type spec = As_part.kind As_part.t ->
  (As_path.t * As_path.rel option) list As_conf.value

val all : spec
val all_output : spec
val all_input : spec
val file_exts : As_path.ext list -> spec
val bin : spec
val lib : spec
val doc : spec

(** {1 Dir} *)

val v : ?usage:As_part.usage -> ?exists:bool As_conf.value -> ?args:As_args.t ->
  ?spec:spec -> ?install:bool -> kind ->
  [< `Base | `Bin | `Dir | `Doc | `Lib | `Unit ] As_part.t list ->
  [> `Dir ] As_part.t
