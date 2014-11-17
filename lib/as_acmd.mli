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

(** Build action commands

    See {!Assemblage.Acmd}. *)

(** {1 Action commands} *)

type cmd
val cmd : string As_conf.key -> cmd As_conf.value
val static : string -> cmd

type t

val v : ?stdin:As_path.t -> ?stdout:As_path.t -> ?stderr:As_path.t -> cmd ->
    string list -> t

val cmd_key : t -> string As_conf.key option
val cmd_name : t -> string
val args : t -> string list
val stdin : t -> As_path.t option
val stdout : t -> As_path.t option
val stderr : t -> As_path.t option

val pp : Format.formatter -> t -> unit
val ctx : As_ctx.t -> t -> As_ctx.t
val args_with_ctx : As_conf.t -> As_ctx.t -> As_args.t -> t -> string list

(** Action command arguments combinators. *)
module Args : sig
  val add : 'a -> 'a list -> 'a list
  val adds : 'a list -> 'a list -> 'a list
  val add_if : bool -> 'a -> 'a list -> 'a list
  val adds_if : bool -> 'a list -> 'a list -> 'a list
  val fadd_if : bool -> ('b -> 'a) -> 'b -> 'a list -> 'a list
  val fadds_if : bool -> ('b -> 'a list) -> 'b -> 'a list -> 'a list
  val path_arg : ?opt:string -> As_path.t -> string list -> string list
  val path_args : ?opt:string ->  As_path.t list -> string list -> string list
  val path : As_path.t -> ext:As_path.ext -> As_path.t
end

(** {1 Portable system utility invocations} *)

val dev_null : As_path.t As_conf.value
val cd : (As_path.t -> t) As_conf.value
val ln : (As_path.t -> As_path.t -> t) As_conf.value
val ln_rel : (As_path.t -> As_path.t -> t) As_conf.value
val cp : (As_path.t -> As_path.t -> t) As_conf.value
val mv : (As_path.t -> As_path.t -> t) As_conf.value
val rm_files : (?f:bool -> As_path.t list -> t) As_conf.value
val rm_dirs : (?f:bool -> ?r:bool -> As_path.t list -> t) As_conf.value
val mkdir : (As_path.t -> t) As_conf.value
val stamp : (As_path.t -> string -> t) As_conf.value
