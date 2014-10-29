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

(** Build argument bundles

    For documentation see {!Assemblage.Args}. *)

(** {1 Arguments with conditions} *)

type cargs
val cond : cargs -> bool As_conf.value
val args : cargs -> string list As_conf.value

(** {1 Argument bundles} *)

type t
val v : ?cond:bool As_conf.value -> As_ctx.t -> string list As_conf.value -> t
val vc : ?cond:bool As_conf.value -> As_ctx.t -> string list -> t
val empty : t
val is_empty : t -> bool
val append : t -> t -> t
val ( @@@ ) : t -> t -> t
val concat : t list -> t
val bindings : t -> (As_ctx.t * cargs list) list
val for_ctx : t -> As_ctx.t -> cargs list

(** {1 Built-in argument bundles} *)

val linkall : t
val thread : t
val vmthread : t
val cclib : string list -> t
val ccopt : string list -> t
val stub : string -> t
val debug : t
val annot : t
val warn_error : t
