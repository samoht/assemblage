(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

(** Generate `.merlin` files. *)

(** Merlin directives *)
module Directive : sig
  (** {1 Variables} *)

  type t
  (** The type for directives. *)

  val s : string -> t
  (** [s dir] is the merlin source directory directive S. *)

  val b : string -> t
  (** [b dir] is the merlin build directory directive B. *)

  val pkg : string -> t
  (** [pkg name] is the merlin findlib package reference directive PKG. *)

  val ext : string -> t
  (** [ext pps] is the merlin syntax extension directive EXT. *)

end

type t
(** The type for `.merlin` files. *)

val create :
  ?s:string list   ->
  ?b:string list   ->
  ?pkg:string list ->
  ?ext:string list ->
  unit -> t
(** Create a `.merlin` file. *)

val add_directive : t -> Directive.t -> t
(** Add a [Directive.t] to a `.merlin` file. *)

val of_project : build_dir:string -> As_project.t -> t
(** Create a `.merlin` file from a project. *)

val to_string : t -> string
(** [to_string m] is a `.merlin` file from [m]. *)

val write_file : string -> t -> unit
(** [write_file file m] write [m] to [file]. *)
