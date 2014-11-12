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

(** API documentation part.

    See {!Assemblage.Doc}. *)

(** {1 Metadata} *)

type kind = [ `OCamldoc ]

val pp_kind : Format.formatter -> kind -> unit

val kind : [< `Doc] As_part.t -> [`OCamldoc ]
val ocamldoc : 'a As_part.t -> [> `Doc ] As_part.t option

(** {1 Unit filters} *)

val default : [< `Unit] As_part.t -> bool
val dev : [< `Unit] As_part.t -> bool

(** {1 Doc} *)

val v : ?usage:As_part.usage -> ?cond:bool As_conf.value ->
  ?args:As_args.t -> ?keep:([< `Unit] As_part.t -> bool) ->
  string -> kind -> [< `Lib | `Unit | `Bin | `Pkg ] As_part.t list ->
  [> `Doc] As_part.t

val of_base : kind -> [< `Base] As_part.t -> [> `Doc ] As_part.t
