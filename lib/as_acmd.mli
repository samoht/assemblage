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

open Bos

type cmd
val cmd : string As_conf.key -> cmd As_conf.value
val static : string -> cmd

type t

val v : ?stdin:path -> ?stdout:path -> ?stderr:path -> cmd ->
    string list -> t

val cmd_key : t -> string As_conf.key option
val cmd_name : t -> string
val args : t -> string list
val stdin : t -> path option
val stdout : t -> path option
val stderr : t -> path option

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
  val path_arg : ?opt:string -> path -> string list -> string list
  val path_args : ?opt:string ->  path list -> string list -> string list
  val path : path -> ext:Path.ext -> path
end

(** {1 Portable system utility invocations} *)

val dev_null : path As_conf.value
val cd : (path -> t) As_conf.value
val ln : (path -> path -> t) As_conf.value
val ln_rel : (path -> path -> t) As_conf.value
val cp : (path -> path -> t) As_conf.value
val mv : (path -> path -> t) As_conf.value
val rm_files : (?f:bool -> path list -> t) As_conf.value
val rm_dirs : (?f:bool -> ?r:bool -> path list -> t) As_conf.value
val mkdir : (path -> t) As_conf.value
val stamp : (path -> string -> t) As_conf.value
