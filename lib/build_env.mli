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

(** Global project environment. The build environment (which can be an
    human) discover available feature. *)

open Project

type t
(** Environment values. *)

val create:

  ?features:(Feature.t * bool) list ->
  (** Project features. *)

  ?comp: string list ->
  (** Additional compilation flags passed to both [ocamlc] and
      [ocamlopt]. *)

  ?bytcomp: string list ->
  (** Additional compilation flags passed to [ocamlc] only. *)

  ?natcomp: string list ->
  (** Additional compilation flags passed to [ocamlopt] only. *)

  ?link: string list ->
  (** Additional link flags passed to both [ocamlc] and [ocamlopt]. *)

  ?bytlink: string list ->
  (** Addtional link flags passed to [ocamlc]. *)

  ?natlink: string list ->
  (** Addtional link flags passed to [ocamlopt]. *)

  ?pp: string list ->
  (** Addition flags passed to the pre-preprocessor. *)

  ?includes: string list ->
  (** List of directories to include when loading `configure.ml'. *)

  ?auto_load: bool ->
  (** Automatically include $(ocamlfind query tools). *)

  ?build_dir: string ->
  (** Location of the generated files. [None] means the files stays in
      the same directory. *)

  ?name:string ->
  (** The package name. *)

  ?version: string ->
  (** The package version. *)

  unit -> t

val default: t
(** Default project configuration. *)

val parse: ?doc:string -> ?man:string list -> string -> Feature.Set.t -> t
(** [parse name features] parse the arguments given on the
    command-line as a configuration value, for the project [name] with
    the possible features [features]. *)

val comp: t -> string list
val bytcomp: t -> string list
val natcomp: t -> string list
(** Return the global comand-line flags for compilation. *)

val link: t -> string list
val bytlink: t -> string list
val natlink: t -> string list
(** Return the global command-line flags for linking. *)

val pp: t -> string list
(** Return the global command-line option for the pre-preprocessor. *)

val build_dir: t -> string
(** Return the directory where build artififacts are generated. *)

val enable: t -> Feature.t list -> bool
(** Check if the given set of flags are all enabled. *)

val includes: t -> string list
(** Return the list of directories to include when loading `configure.ml'. *)

val auto_load: t -> bool
(** Automatically include $(shell ocamlfind query tools) before
    loading `configure.ml'. *)

val features: t -> (Feature.t * bool) list
(** Return a list of feature with the values they are set to. *)

val name: t -> string option
(** Return the package name. *)

val version: t -> string option
(** Return the package version. *)
