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

(** Main entry points. *)

type tool = Project.t -> Build_env.t -> unit
(** The signature of tools. *)

val process: ?file:string -> string -> tool -> unit
(** [process ~file name fn] reads and process the OCaml [file] in a
    top-level environment, for the project called [name], and apply
    [fn] to the projects registered as side-effects. *)

val configure: [`Make] -> tool
(** Configure the project by generating the build, META and .install
    files, using the given build system backend (currently, only GNU
    make is supported). *)

val describe: tool
(** Describe the project to stdout. *)
