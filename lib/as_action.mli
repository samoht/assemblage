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

(** {1 Products} *)

type product = As_path.rel As_conf.value
type products = As_path.rel list As_conf.value

(** {1 Build commands} *)

type cmd
val cmd_cmd : cmd -> string
val cmd_args : cmd -> string list
val cmd_args_with_ctx : As_conf.t -> As_ctx.t -> As_args.t -> cmd -> string list
val cmd_stdin : cmd -> As_path.rel option
val cmd_stdout : cmd -> As_path.rel option
val cmd_stderr : cmd -> As_path.rel option

type cmds = cmd list As_conf.value

type cmd_gen =
  ?stdin:As_path.rel -> ?stdout:As_path.rel -> ?stderr:As_path.rel ->
  string list -> cmd

val cmd : string As_conf.key -> cmd_gen As_conf.value
val cmd_exec : ?stdin:product -> ?stdout:product -> ?stderr:product ->
  string As_conf.key -> string list As_conf.value -> cmds

val seq : cmds -> cmds -> cmds
val ( <*> ) : cmds -> cmds -> cmds

(** {2 Portable system utility invocations} *)

val dev_null : As_path.t As_conf.value

val ln : (As_path.rel -> As_path.rel -> cmd) As_conf.value
val cp : (As_path.rel -> As_path.rel -> cmd) As_conf.value
val mv : (As_path.rel -> As_path.rel -> cmd) As_conf.value
val rm_files : (?f:bool -> As_path.rel list -> cmd) As_conf.value
val rm_dirs : (?f:bool -> ?r:bool -> As_path.rel list -> cmd) As_conf.value
val mkdir : (As_path.rel -> cmd) As_conf.value

(** {1 Actions} *)

type t

val v : ?cond:bool As_conf.value -> ctx:As_ctx.t -> inputs:products ->
  outputs:products -> cmds -> t

val cond : t -> bool As_conf.value
val ctx : t -> As_ctx.t
val inputs : t -> products
val outputs : t -> products
val cmds : t -> cmds
val deps : t -> As_conf.Key.Set.t

(** Combinators to define build actions.

    See {!Assemblage.Action.Spec}. *)
module Spec : sig

  (* List configuration values *)

  type 'a list_v = 'a list As_conf.value

  val atom : 'a -> 'a list_v
  val atoms : 'a list ->  'a list_v
  val add : 'a list_v -> 'a list_v -> 'a list_v
  val add_if : bool As_conf.value -> 'a list_v -> 'a list_v -> 'a list_v
  val add_if_key : bool As_conf.key -> 'a list_v -> 'a list_v -> 'a list_v

  (* Paths and products *)

  val path : product -> ext:As_path.ext -> product
  val path_base : product -> string As_conf.value
  val path_dir : As_path.rel As_conf.value -> As_path.rel As_conf.value
  val path_arg : ?opt:string -> As_path.rel As_conf.value -> string list_v
  val paths_args : ?opt:string -> As_path.rel list As_conf.value ->
    string list_v

  val product : ?ext:As_path.ext -> product -> products

  (* Commands *)

  val ( <*> ) : cmds -> cmds -> cmds
end

val link : src:product -> dst:product -> unit -> t
