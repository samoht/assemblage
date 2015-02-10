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

(** Executing (non build) commands and IO operations.

    For documentation see {!Assemblage.Cmd}. *)

(** {1 Command results} *)

type 'a result = [ `Ok of 'a | `Error of string ]

val ret : 'a -> 'a result
val error : string -> 'a result
val bind : 'a result -> ('a -> 'b result) -> 'b result
val map : 'a result -> ('a -> 'b) -> 'b result
val get : 'a result -> 'a
val on_error : ?level:As_log.level -> use:'a -> 'a result -> 'a
val ignore_error : use:'a -> 'a result -> 'a
val reword_error : ?replace:bool -> string -> 'a result -> 'a result
val exn_error : ?msg:(Printexc.raw_backtrace -> exn -> 'a -> string) ->
  (('a -> 'b) -> ('a -> 'b result))

val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result

module Infix : sig
  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
  val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
end

(** {1 Path, files, directories and version control systems} *)

type path = As_path.t (* to avoid assemblage.mli confusion *)

module Path : sig
  val exists : As_path.t -> bool result
  val move : ?force:bool -> As_path.t -> As_path.t -> unit result
end

module File : sig
  val dev_null : As_path.t
  val exists : As_path.t -> bool result
  val delete : ?maybe:bool -> As_path.t -> unit result
  val temp : ?dir:As_path.t -> string -> As_path.t result
  val with_inf : (in_channel -> 'a -> 'b result) -> As_path.t -> 'a -> 'b result
  val read : As_path.t -> string result
  val read_lines : As_path.t -> string list result

  val with_outf : (out_channel -> 'a -> 'b result) -> As_path.t -> 'a ->
    'b result
  val write : As_path.t -> string -> unit result
  val write_lines : As_path.t -> string list -> unit result
  val write_subst : (string * string) list -> As_path.t -> string -> unit result
end

module Dir : sig
  val exists : As_path.t -> bool result
  val getcwd : unit -> As_path.t result
  val chdir : As_path.t -> unit result
  val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) ->
    'a -> string list -> 'a result
end

module Vcs : sig
  type t = [ `Git | `Hg ]

  val override_kind : unit -> t option
  val set_override_kind : t option -> unit
  val override_exec : unit -> string option
  val set_override_exec : string option -> unit

  val exists : As_path.t -> t -> bool result
  val find : As_path.t -> t option result
  val get : As_path.t -> t result
  val head : ?dirty:bool -> As_path.t -> t -> string result
  val describe : ?dirty:bool -> As_path.t -> t -> string result
end

(** {1 Environment variable lookup} *)

val env : string -> string option
val get_env : string -> string result

(** {1 Executing commands} *)

val exists : string -> bool result
val exec_ret : string -> string list -> int
val exec : string -> string list -> unit result
val read : ?trim:bool -> string -> string list -> string result
val read_lines : string -> string list -> string list result
val write : string -> string list -> As_path.t -> unit result
