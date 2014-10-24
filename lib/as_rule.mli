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

(** Build rules and products.

    See {!Assemblage.Rule}. *)

(** {1 Action} *)

type cmd = As_args.t * (string list -> string list)
val cmd : string list -> cmd

type action = cmd list

(** {1 Rules} *)

type t

val create :
  context:As_context.t ->
  inputs:As_product.t list ->
  outputs:As_product.t list ->
  action:action -> t

val context : t -> As_context.t
val inputs : t -> As_product.t list
val outputs : t -> As_product.t list
val action : t -> action

(** {1 Predicates} *)

val has_context : As_context.t -> t -> bool

(** {1 Built-in rules} *)

val link : ?cond:bool As_conf.value -> ?args:As_args.t -> As_env.t ->
  src:As_path.rel -> dst:As_path.rel -> t

val mkdir : ?cond:bool As_conf.value -> ?args:As_args.t -> As_env.t ->
  dir:As_path.rel -> t
