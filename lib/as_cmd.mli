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
val on_error : ?level:As_log.level -> use:'a -> 'a result -> 'a
val bind : 'a result -> ('a -> 'b result) -> 'b result
val map : 'a result -> ('a -> 'b) -> 'b result
val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result

module Infix : sig
  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
  val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
end

(** {1 Working with files, directories and version control systems} *)

module File : sig
  val exists : As_path.t -> bool result
  val null : As_path.t
  val with_inf : (in_channel -> 'a -> 'b result) -> As_path.t -> 'a -> 'b result
  val with_outf : (out_channel -> 'a -> 'b result) -> As_path.t -> 'a ->
    'b result

  val input : As_path.t -> string result
  val input_lines : As_path.t -> string list result
  val output : As_path.t -> string -> unit result
  val output_lines : As_path.t -> string list -> unit result
  val output_subst : (string * string) list -> As_path.t -> string ->
    unit result

  val delete : ?maybe:bool -> As_path.t -> unit result
  val temp : string -> As_path.t result
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

  val set_override : t option -> unit
  val set_override_exec : string option -> unit

  val exists : As_path.t -> t -> bool result
  val find : As_path.t -> t option result
  val get : As_path.t -> t result
  val head : ?dirty:bool -> As_path.t -> t -> string result
  val describe : ?dirty:bool -> As_path.t -> t -> string result
end

(** {1 Executing commands} *)

val set_trace : bool -> unit

val exists : string -> bool result
val exec_ret : string -> string list -> int result
val exec : string -> string list -> unit result
val input : ?trim:bool -> string -> string list -> string result
val input_lines : string -> string list -> string list result
val output : string -> string list -> As_path.t -> unit result
