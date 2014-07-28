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

(** Global project environment.

    The build environment (which can be an human) discovers available
    features. *)

type t
(** Environment values. *)

val create:

  ?features:(As_features.atom * bool) list ->
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
  (** List of directories to include when loading `assemble.ml'. *)

  ?auto_load: bool ->
  (** Automatically include $(ocamlfind query tools). *)

  ?build_dir: string ->
  (** Location of the generated files. [None] means the files stays in
      the same directory. *)

  unit -> t

val default: t
(** Default project configuration. *)

val parse: ?doc:string -> ?man:string list -> string -> As_features.t -> t
(** [parse name features] parse the arguments given on the
    command-line as a configuration value, for the project [name] with
    the possible features [features]. *)

val flags: t -> As_flags.t
(** Return the global comand-line flags. *)

val build_dir: t -> string
(** Return the directory where build artififacts are generated. *)

val enable: t -> As_features.atom list -> bool
(** Check if the given set of flags are all enabled. *)

val includes: t -> string list
(** Return the list of directories to include when loading `assemble.ml'. *)

val auto_load: t -> bool
(** Automatically include $(shell ocamlfind query tools) before
    loading `assemble.ml'. *)

val features: t -> (As_features.atom * bool) list
(** Return a list of feature with the values they are set to. *)
