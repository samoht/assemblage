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

type t = Project.t
(** The type for descriptions of full OCaml projects. *)

type cu = Project.CU.t
(** The type for description of compilation units. *)

type lib = Project.Lib.t
(** The type for description of of libraries. *)

type bin = Project.Bin.t
(** The type for description of binaries. *)

type test = Project.Test.t
(** The type for description of tests. *)

type js = Project.JS.t
(** The type for description of [js_of_ocaml] artifacts. *)

type c = Project.C.t
(** The type for description of C source files. *)

type gen = Project.Gen.t
(** The type for generated OCaml source code. *)

type component =
  [ `CU of cu
  | `Lib of lib
  | `Pp of lib
  | `Pkg_pp of string
  | `Pkg of string
  | `Bin of bin
  | `C of c
  | `JS of js
  | `Test of test
  | `Gen of gen ]
(** The type for all the possible component descriptions. A [`CU cu]
    is a local compilation unit, [`Lib l] are local libraries, [`Pp p]
    are local pre-processors and [`Bin] are local binaries.

    [`Pkg p] are globally installed packages and [`Pkg_pp] are
    globally installed pre-processor packages, both usually managed by
    {i ocamlfind}, *)

(** {2 The Project API} *)

val cu: ?dir:string -> component list -> string -> [> `CU of cu]
(** [cu ~lib ~dir deps name] is the compilation unit in the bag of
    compilation units [bag], located in the directory [dir], which the
    dependencies [deps] and the cname [name]. The name is the same as
    the filename, without its extension. *)

val ocamldep: dir:string -> ?flags:Flags.t -> component list -> [> `CU of cu] list
(** [ocamldep ~dir] is the list of compilation units in the given
    directory, obtained by running [ocamldep] with the given flags. *)

val generated: ?action:(Resolver.t -> Action.t) ->
  component list -> [`Both|`ML|`MLI] -> string -> [> `Gen of gen]
(** Generated OCaml source file(s). *)

val c:
  ?dir:string ->
  ?link_flags:string list ->
  component list -> string list -> string -> [> `C of c]
(** [c deps libs name] is the C file [name.c], which need the C
    libraries [libs] to be compiled -- and it has the dependencies
    [deps]. *)

val cstubs:
  ?dir:string ->
  ?available:Feature.formula ->
  ?headers:string list ->
  ?cflags:string list ->
  ?clibs:string list ->
  component list -> string -> [> `Lib of lib]
(** [stubs deps name] is the C stub generations, using Ctypes, of the
    compilation unit [name]. *)

val lib:
  ?available:Feature.formula ->
  ?flags:Flags.t ->
  ?pack:bool ->
  ?deps:component list -> [`CU of cu] list -> string -> [> `Lib of lib]
(** [lib cus name] is the library [name] composed by the compilation
    units [cus]. If [lib] is set, use [ocamldep] to approximate the
    compilation units and their dependecies in the given directory. *)

val bin:
  ?dir:string ->
  ?byte_only:bool ->
  ?link_all:bool ->
  ?install:bool ->
  component list -> string list -> string -> [> `Bin of bin]
(** [bin deps units name] is the binary [name] obtained by compiling
    the compilation units [units], with the dependencies [deps]. By
    default, the source files are located into {i bin/} (this is
    controled by the value of [dir]). *)

val js: [`Bin of bin] -> string list -> [> `JS of js]
(** [js bin args] is the decription of a javascript artefact generated
    by [js_of_ocaml]. *)

val pkg: string -> [> `Pkg of string]
(** An external package. *)

val pkg_pp: string -> [> `Pkg_pp of string]
(** An external pre-processor. *)

val test: ?dir:string ->
  component list -> Project.Test.command list -> string -> [> `Test of test]
(** Description of a test. *)

val test_bin: [`Bin of bin] -> Project.Test.args -> Project.Test.command
(** A test which runs a binary built in the project. *)

val test_shell: ('a, unit, string, Project.Test.command) format4 -> 'a
(** A test which runs an arbitrary shell command. *)

val create:
  ?flags:Flags.t ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?version:string ->
  component list -> string -> unit
(** [create deps name] registers the project named [name], defining
    the libraries, binaries and tests defined by the transitive
    closure of objects in [deps]. *)

(** {1:features Features} *)

(** {1:flags Flags} *)

(** {1:tools Tools} *)

type tool = Project.t -> Build_env.t -> unit
(** The signature of tools. *)

val process: ?file:string -> string -> tool -> unit
(** [process ~file name fn] reads and processes the OCaml [file] in a
    top-level environment, for the project called [name], and apply
    [fn] to the projects registered as side-effects. *)

val configure: [`Make] -> tool
(** Configure the project by generating the build, META and .install
    files, using the given build system backend (currently, only GNU
    make is supported). *)

val describe: tool
(** Describe the project to stdout. *)
