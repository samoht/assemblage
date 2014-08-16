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

(** Project

    A datastructure to describe projects. Models a project as a
    description of a set of components such as libraries, binaries,
    tests, etc. forming a DAG. *)

(** {1 Projects} *)

type t
(** The type for describing projects. *)

val create : ?available:As_features.t -> ?flags:As_flags.t ->
  ?version:string -> string -> As_component.t list -> t
(** [create cs n] is the project named [n] with components [cs]. *)

val components : t -> As_component.t list
(** Return the project components in context. *)

val name : t -> string
(** [name t] is the project name. *)

val version : t -> string
(** [version t] is the project version. *)

val features : t -> As_features.Set.t
(** [features t] is the collection of features used in the project. *)
