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

open Project

val process: ?file:string -> ?name:string -> (t -> Build_env.t -> unit) -> unit
(** [process ~file fn] reads and process the OCaml [file] in a
    top-level environment, where the [tools] API has been loaded, and
    apply [fn] to the projects registered as side-effects. By default,
    [file] is `configure.ml' and [name] is [Sys.argv.(0)]. *)

val generate: t -> Build_env.t -> [`Makefile] -> unit
(** Generate the project files, using the given build system
    backend. *)

val describe: t -> Build_env.t -> unit
(** Describe the project to stdout. *)
