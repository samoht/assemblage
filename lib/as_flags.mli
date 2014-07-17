(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Command line arguments *)

type args = string list
(** The type for partial command lines arguments. *)

type t
(** The type for contextualized, partial, command line arguments. *)

val create :
  ?available:As_features.t ->
  ?pp_byte:args ->
  ?pp_native:args ->
  ?comp_byte:args ->
  ?comp_native:args ->
  ?comp_js:args ->
  ?link_byte:args ->
  ?link_native:args ->
  ?link_js:args ->
  ?link_shared:args ->
  ?c:args -> unit -> t
(** Create a full command line argument using the the given single
    command line arguments. *)

val (@@@): t -> t -> t
(** Append command line flags. *)

(* FIXME: field accessors need an Features evaluation context. *)

val pp_byte: t -> args
(** The command line arguments for pre-processing files in bytecode
    mode. *)

val pp_native: t -> args
(** The command line arguments for pre-processing files in native
    mode. *)

val comp_byte: t -> args
(** The command line arguments for compiling compilation units in
    bytecode mode. *)

val comp_native: t -> args
(** The command line arguments for compiling compilation units in
    native mode. *)

val comp_js: t -> args
(** The command line arguments for compiling compilation units in
    JavaScript mode. *)

val link_byte: t -> args
(** The command line arguments to link compilation units in bytecode
    mode. *)

val link_native: t -> args
(** The command line arguments to link compilation units in native
    mode. *)

val link_js: t -> args
(** The command line arguments to link compilation units in
    JavaScript mode. *)

val link_shared: t -> args
(** The command line arguments to link shared libraries. *)

val c: t -> args
(** The command line arguments to pass to the C compiler. *)

(** {1 Built-in flags} *)

val empty : t
val debug : t
val annot : t
val warn_error : t
val linkall : t
val thread : t
val cclib : string list -> t
val ccopt : string list -> t
val stub : string -> t
