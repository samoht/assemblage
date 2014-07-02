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

(** Dependency resolver. *)

type t
(** Resolver values. *)

val create: buildir:(string -> string) -> pkgs:(string list -> Flags.t) -> t
(** [create ~buildir ~pkgs] is the resolver which apply the function
      [buildir] to resolve local libraries and resolves a set of
      global package by applying [pkgs]. *)

val build_dir: t -> string -> string
(** Resolve locally generated filename (by usually prepending the
    build directory name). *)

val pkgs: t -> string list -> Flags.t
(** Resolve global package names into command-line flags. *)
