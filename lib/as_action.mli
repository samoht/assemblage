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

(** {1 Actions} *)

open Bos

type t

val v : ?log:string -> ?ctx:As_ctx.t -> ?inputs:path list ->
  ?outputs:path list -> As_acmd.t list -> t

val ctx : t -> As_ctx.t
val inputs : t -> path list
val outputs : t -> path list
val cmds : t -> As_acmd.t list
val args : t -> As_args.t
val log : t -> string option
val products : t -> path list

val add_cmds : [`Before | `After] -> As_acmd.t list -> t -> t
val add_ctx_args : As_ctx.t -> As_args.t -> t -> t
(** [add_ctx_args ctx args t] adds context [ctx] and argument bundle [args]
    to [t]. This is used by parts to watermark their actions
    on {!As_part.actions}. *)

val pp : As_conf.t -> Format.formatter -> t -> unit

(** {1 Action lists} *)

val list_inputs : t list -> path list
val list_outputs : t list -> path list
val list_products : t list -> path list

(** {1 Build actions} *)

val symlink : (path -> path -> t) As_conf.value
