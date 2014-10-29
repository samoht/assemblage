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

(** Build products.

    For documentation see {!Assemblage.Product}. *)

(** {1 Build products} *)

type t = [ `File of As_path.rel | `Effect of As_path.rel ]

val path : t -> As_path.rel
val raw_path : t -> string
val basename : t -> string
val dirname : t -> As_path.rel

(** {1 Predicates} *)

val is_file : t -> bool
val is_effect : t -> bool
val has_ext : As_path.ext -> t -> bool
val keep_ext : As_path.ext -> t -> [`File of As_path.rel ] option

(** {1 Converting to arguments} *)

(*
val target_to_args : ?pre:string list -> As_context.t list -> t -> As_args.t
val dirname_to_args : ?pre:string list -> As_context.t list -> t -> As_args.t
*)
