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

(** Build actions.

    See {!Assemblage.Action}. *)

(** {1 Build commands} *)

type cmd
val cmd : string As_conf.key -> string list As_conf.value -> cmd
val seq : cmd -> cmd -> cmd
val ( <*> ) : cmd -> cmd -> cmd

(** {1 Actions} *)

type t

val create :
  ?cond:bool As_conf.value ->
  ?ctx:As_ctx.t ->
  ?args:As_args.t ->
  inputs:As_product.t list As_conf.value ->
  outputs:As_product.t list As_conf.value ->
  cmd -> t

val cond : t -> bool As_conf.value
val ctx : t -> As_ctx.t
val args : t -> As_args.t
val inputs : t -> As_product.t list As_conf.value
val outputs : t -> As_product.t list As_conf.value
val cmd : t -> cmd
