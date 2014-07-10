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

(** The Assemblage Library.

    [Assemblage] provides a simple embedded domain specific language
    to describe [OCaml] {{!project}projects}. It also provides simple
    {{!tools}tools} to configure, manage, install and use OCaml
    projects.

    Describing a project eventually leads to describe its build
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

(** {1:features Features} *)

(** Typical OCaml project might have multiple features, which modifies
    the list of generated artifacts. An example is a project which
    depends on either [lwt] or [async], and which choose to compile
    the relevant libraries only if one of the other is installed on
    the system. Other typical feature might depend on the machine the
    project is running on, for instance the fact that the native
    toolchain is available or not. *)

module Features: sig

  (** Features. *)

  type t
  (** The type for user-defined or system-dependent features. *)

  val create: doc:string -> default:bool -> string -> t
  (** Create a feature. *)

  val not_: t -> t
  (** Negate a feature formulae. *)

  val (&&&): t -> t -> t
  (** Logical AND of feature formulaes. *)

  val (|||): t -> t -> t
  (** Logical OR of feature formulaes. *)

  val native: t
  (** Is native-code enabled? *)

  val native_dynlink: t
  (** Is dynlink for native code enabled? *)

  val annot: t
  (** Generate annot files? *)

  val debug: t
  (** Generate debug symbols? *)

  val warn_error: t
  (** Consider warning as error? *)

  val test: t
  (** Compile and run tests? *)

  val doc: t
  (** Build the documentation? *)

  val js: t
  (** Build the javascript objects? *)

end

(** {1:flags Flags} *)

(** The various compilation options that are set for a project
    eventually reduce to tweaking the simple command-line arguments to
    the compiler. We distinguish three different phases, where the
    command-line can be changed: the pre-processing step, the separate
    compilation of module implementations and signatures and the
    linking of such compilation units to produce a library or a
    binary. These three phases come in two modes: bytecode compilation
    and native-code compilation. *)

module Flags: sig

  (** Flags. *)

  type t
  (** The type for command-line arguments. *)

  val create:
    ?comp_byte:string list ->
    ?comp_native:string list ->
    ?pp_byte:string list ->
    ?pp_native:string list ->
    ?link_byte:string list ->
    ?link_native:string list ->
    ?link_shared:string list ->
    ?c:string list ->
    unit -> t
  (** Create a full command-line argument using the the given single
      command-line arguments. *)

  val (@@@): t -> t -> t
  (** Append command-line flags. *)

  val empty: t
  (** Empty flags. *)

  val debug: t
  (** The [-g] flags. *)

  val annot: t
  (** The [-bin-annot] flags. *)

  val warn_error: t
  (** The [-warn-error] flags. *)

  val linkall: t
  (** The [-linkall] flags. *)

  val thread: t
  (** The [-thread] flags. *)

  val cclib: string list -> t
  (** The [-cclib x] flags. *)

  val ccopt: string list -> t
  (** The [-ccopt x] flags. *)

  val stub: string -> t
  (** [stub s] adds {i -cclib -l[s] -dllib -l[s]} to the bytecode
      linking options and {i -cclib -l[s]} to the native linking
      options. *)

end

(** {1:project Project} *)

type t
(** The type for OCaml projects descriptions. *)

type cu
(** The type for compilation unit descriptions. *)

type lib
(** The type for library descriptions. *)

type bin
(** The type for binary executable descriptions. *)

type test
(** The type for test descriptions. *)

type js
(** The type for [js_of_ocaml] artifact descriptions. *)

type c
(** The type for C source file descriptions. *)

type gen
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
    is a local compilation unit, [`Lib l] is a local library, [`Pp p]
    is a local pre-processors and [`Bin] is local biny.

    [`Pkg p] is a globally installed packages and [`Pkg_pp] is a
    globally installed pre-processor packages, both usually managed by
    {i ocamlfind}. *)

(** {2 The Project API} *)

val cu: ?dir:string -> component list -> string -> [> `CU of cu]
(** [cu ~dir deps name] is the compilation unit located in the
    directory [dir] with dependencies [deps] and the cname [name]. The
    name is the same as the filename, without its extension. *)

val ocamldep: dir:string -> ?flags:Flags.t -> component list -> [> `CU of cu] list
(** [ocamldep ~dir] is the list of compilation units in the given
    directory, obtained by running [ocamldep] with the given flags. *)

val generated: ?action:(As_resolver.t -> As_action.t) ->
  component list -> [`C|`ML|`MLI] list -> string -> [> `Gen of gen]
(** Generated OCaml source file(s). The custom action get the name of
    the build dir as argument. *)

val c:
  ?dir:string ->
  ?link_flags:string list ->
  component list -> string list -> string -> [> `C of c]
(** [c deps libs name] is the C file [name.c], which need the C
    libraries [libs] to be compiled -- and it has the dependencies
    [deps]. *)

val cstubs:
  ?dir:string ->
  ?available:Features.t ->
  ?headers:string list ->
  ?cflags:string list ->
  ?clibs:string list ->
  component list -> string -> [> `Lib of lib]
(** [stubs deps name] is the C stub generations, using Ctypes, of the
    compilation unit [name]. *)

val lib:
  ?available:Features.t ->
  ?flags:Flags.t ->
  ?pack:bool ->
  ?deps:(string -> component list) ->
  ?c:[`C of c] list ->
  [`CU of cu] list -> string -> [> `Lib of lib]
(** [lib units name] is the library [name] composed by the compilation
    units [cus]. If [lib] is set, use [ocamldep] to approximate the
    compilation units and their dependecies in the given directory. *)

val bin:
  ?byte_only:bool ->
  ?link_all:bool ->
  ?install:bool ->
  ?deps:(string -> component list) ->
  [`CU of cu] list -> string -> [> `Bin of bin]
(** [bin units name] is the binary [name] obtained by compiling
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

type test_command
(** The type for test commands. *)

type test_args = (component -> string) -> string list
(** The type for command-line arguments when calling tests of executable. *)

val test_bin: [`Bin of bin] -> ?args:test_args -> unit -> test_command
(** A test which runs a binary built in the project. *)

val test_shell: ('a, unit, string, test_command) format4 -> 'a
(** A test which runs an arbitrary shell command. *)

val test: ?dir:string ->
  component list -> test_command list -> string -> [> `Test of test]
(** Description of a test. *)

val create:
  ?flags:Flags.t ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?version:string ->
  component list -> string -> unit
(** [create deps name] registers the project named [name], defining
    the libraries, binaries and tests defined by the transitive
    closure of objects in [deps]. *)

(** {1:tools Tools} *)

type tool = t -> As_build_env.t -> unit
(** The signature of tools. *)

val process: ?file:string -> string -> tool -> unit
(** [process ~file name fn] reads and processes the OCaml [file] in a
    top-level environment (the default is [configure.ml]), for the
    project called [name], and apply [fn] to the projects registered
    as side-effects. *)

val configure: [`Make] -> tool
(** Configure the project by generating the build, META and .install
    files, using the given build system backend (currently, only GNU
    make is supported). *)

val describe: tool
(** Describe the project to stdout. *)
