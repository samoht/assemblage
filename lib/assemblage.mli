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

(** The Assemblage Library. *)

(** [Assemblage] provides a simple embedded domain specific language
    to describe [OCaml] {{!project}projects}. It also provides simple
    {{!tools}tools} to configure, manage, install and use OCaml
    projects.

    Describing a project is eventually leads to describe its build
    artifacts. The exact mapping betweem project elements and those
    build artifacts depends on the presence or absence of
    {{!features}features}. Examples of such features are weather to
    compile and run the tests (user-defined feature), the presence of
    the native compiler (system-dependant feature), compiling a
    library using [async] or [lwt] as a backend (environment-dependant
    feature), ...

    Finally, generating build artifacts means refining the project
    description into concrete {{!flags}command-line arguments} to pass
    to the different compilers ([ocamlc], [ocamlopt]) on the different
    compilation and linking phases.

    {e Release %%VERSION%% - %%AUTHOR%% } *)

(** {1:project Project} *)

(** {1:features Features} *)

(** {1:flags Flags} *)

(** {1:tools Tools} *)

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

val timestamp: string
(** Timestamp to write in the generated files. *)
