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

(** Actions to generated source files. *)

type t
(** Generator values. *)

val none: t
(** No action. *)

val func: (unit -> unit) -> t
(** [func fn] is a generator which produces some results by calling
    [fn build_dir]. *)

val shell: ?dir:string -> string -> string list -> t
(** [shell ~dir cmd args] is a generator which produces some results
    by calling [cmd args] in a shell running in the directory
    [dir]. *)

val bash: ?dir:string -> ('a, unit, string, t) format4 -> 'a
(** [bash ~dir fmt] is a generator which produces some results by
    calling [fmt] in a bash shell, running in the directory
    [dir]. *)

val run: t -> unit
(** Run the generator. *)
