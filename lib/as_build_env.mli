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

(** Build environment.

    The build environment (which can be an human) discovers available
    features. *)

type t
(** The type for build environments. *)

val create :
  ?features:(As_features.atom * bool) list ->
  ?flags:As_flags.t ->
  ?build_dir: string ->
  unit -> t
(** {ul
    {- [build_dir] is the location of the generated files. [None]
      means the files stays in the same directory.}} *)

val default : t
(** Default project configuration. *)

val flags : t -> As_flags.t
(** Return the global comand-line flags. *)

val build_dir : t -> string
(** Return the directory where build artifacts are generated. *)

val enable : t -> As_features.atom list -> bool
(** Check if the given set of flags are all enabled. *)

val features : t -> (As_features.atom * bool) list
(** Return a list of feature with the values they are set to. *)

val term : As_features.Set.t -> t Cmdliner.Term.t
(** [term fs] is a command line for features [fs] resulting
    in an environement value. *)
