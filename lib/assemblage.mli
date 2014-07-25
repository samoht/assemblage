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
    {{!Features}features}. Examples of such features are weather to
    compile and run the tests (user-defined feature), the presence of
    the native compiler (system-dependant feature), compiling a
    library using [async] or [lwt] as a backend (environment-dependant
    feature), ...

    Finally, generating build artifacts means refining the project
    description into concrete {{!Flags}command line arguments} to pass
    to the different compilers ([ocamlc], [ocamlopt]) on the different
    compilation and linking phases.

    {e Release %%VERSION%% - %%AUTHOR%% } *)

(** {1:featuresandflags Features and flags} *)

  (** Features.

      Features declare booleans to be determined by the
      {{!buildenv}build environment}. They allow to condition the
      application of certain {{!Flags}flags} and restrict the build of
      project components or of certain of their artifacts.

      Examples of features are: native compilation availability,
      optional package availability, debug build support, etc. *)
module Features : sig

  (** {1 Features} *)

  type set
  (** Set of features. FIXME: remove this. *)

  type t
  (** The type for features. Given a build environment a value of this type
      denotes a boolean value. *)

  val create : ?default:bool -> string -> doc:string -> t
  (** [create default name doc] is a feature named [name].  [default]
      (defaults to [true]) indicates the boolean value the feature
      takes if the build environment doesn't determine it.  [doc] is
      used for documenting the feature in various contexts, keep it
      short and to the point. *)

  val true_ : t
  (** [true_] is always true. *)

  val false_ : t
  (** [false_] is always false. *)

  val not_ : t -> t
  (** [not_ f] is true iff [f] is false. *)

  val (&&&) : t -> t -> t
  (** [f &&& f'] is true iff both [f] and [f'] are true. *)

  val (|||) : t -> t -> t
  (** [f ||| f'] is true iff either [f] or [f'] is true. *)

  (** {1 Built-in features} *)

  val native : t
  (** [native] is true iff native code compilation is available. *)

  val native_dynlink : t
  (** [native_dynlink] is true iff native code dynamic linking is available. *)

  val js : t
  (** [js] is true iff JavaScript compilation is available. *)

  val annot : t
  (** [annot] is true iff binary annotations files must be built. *)

  val debug : t
  (** [debug] is true iff builds must support debugging. *)

  val warn_error : t
  (** [warn_error] is true iff builds must consider warnings as errors. *)

  val test : t
  (** [test] is true iff tests must be built. *)

  val public_doc : t
  (** [public_doc] is true iff the public documentation must be built. *)

  val full_doc : t
  (** [full_doc] is true iff the full documentation must be built.
      FIXME. *)
end

(** Flags

    Flags values denote sets of partial command line arguments given
    to tools in a given context. A context is defined by a {{!phase}phase}
    and a {{!mode}mode}.

    FIXME describe valid context and to what they correspond. *)
module Flags : sig

  (** {1:flags Flags} *)

  type phase = [ `Pp | `Compile | `Link | `Other | `Run | `Test ]
  (** The type for phases. *)

  type mode = [ `Byte | `Native | `Shared | `C | `Js ]
  (** The type for modes. *)

  type args = string list
  (** The type for partial command line arguments. *)

  type t
  (** The type for multi-context, partial, command line arguments. *)

  val v : ?available:Features.t -> phase -> mode -> args -> t
  (** [v available phase mode args] is the partial command line
      [args] in the context defined by [phase] and [modes]. This partial
      command line is only available whenever the feature [available]
      is true (defaults to {!Features.true_}). *)

  val (@@@) : t -> t -> t
  (** [f @@@ f'] concatenates context wise [f'] to [f]. [f'] and
      [f'] remain available according to their own [available] argument. *)

  (** {1 Built-in flags} *)

  val empty : t
  (** [empty] is the command line [[]] for every context.  *)

  val debug : t
  (** [debug] is the debug flag as needed per context, only
      available when {!Features.debug} is true. *)

  val annot : t
  (** [annot] is the [-bin-annot] flag in appropriate contexts, only
      available when {!Features.annot} is true. *)

  val warn_error : t
  (** [warn_error] is the [-warn-error] in appropriate contexts, only
      available when {!Features.warn_error} is true. *)

  val linkall : t
  (** [linkall] is the [-linkall] flag in appropriate contexts. *)

  val thread : t
  (** [thread] is the [-thread] flag in appropriate contexts. *)

  val cclib : string list -> t
  (** The [-cclib x] flags. *)

  val ccopt : string list -> t
  (** The [-ccopt x] flags. *)

  val stub : string -> t
  (** [stub s] adds {i -cclib -l[s] -dllib -l[s]} to the bytecode
      linking options and {i -cclib -l[s]} to the native linking
      options. *)
end

(** {1:resolvers Resolvers} *)

(** A project defines different kinds of names: local and external
    library names. A local library name needs to be associated with
    the name of the local directory where the build artifacts for that
    library are created and an external library name needs to be
    associated with the global directory where the object files for
    that library are installed. *)

module Resolver: sig

  (** Name resolvers. *)

  type t
  (** The type for internal and external name resolvers. *)

  val create : build_dir:string -> pkgs:(string list -> Flags.t) -> t
  (** [create ~buildir ~pkgs] is the resolver which prefixes [buildir]
      to resolve local library names and applies [pkgs] to resolve a set
      of global package names. *)

  val build_dir : t -> string -> string
  (** Resolve locally generated filename by prepending the build
      directory name. *)

  val pkgs : t -> string list -> Flags.t
  (** Resolve global package names into command line flags. *)

end

(** {1:actions Custom Actions} *)

module Action: sig

  (** Actions to generate source files. *)

  type custom
  (** Custom actions. *)

  val custom : ?dir:string -> ('a, unit, string, custom) format4 -> 'a
  (** [bash ~dir fmt] is a generator which produces some results by
      calling [fmt] in a bash shell, running in the directory
      [dir]. *)

  type t = Resolver.t -> custom
  (** Type of custom actions. *)

end

(** {1:comps Components}

    A project is a set of components. Each component describes a
    logical build unit like a compilation unit, a library, a binary, a
    test, the documentation of a library, etc. A component can depend
    on other components, either because it needs the dependencies to
    be part of it or because it needs them to be build. *)

type comp_unit
(** The type for compilation unit descriptions. *)

type other
(** The type for arbitrarily constructed files descriptions. *)

type c
(** The type for C source file descriptions. *)

type js
(** The type for [js_of_ocaml] artifact descriptions. *)

type pkg
(** The type for package descriptions. *)

type lib
(** The type for library descriptions. *)

type bin
(** The type for binary executable descriptions. *)

type files
(** The type for file artifacts descriptions. *)

type test
(** The type for test descriptions. *)

type component =
  [ `Unit of comp_unit
  | `Other of other
  | `C of c
  | `JS of js
  | `Lib of lib
  | `Pkg of pkg
  | `Bin of bin
  | `Files of files
  | `Test of test ]
(** The type for components.
    {ul
    {- [`Unit u] u is a project compilation unit.}
    {- [`Lib l] is a project library.}
    {- [`Bin b] is a project binary.}
    {- [`Test b] is a project test.}
    {- [`Pkg p] is an external named package.}
    {- FIXME}} *)

val unit : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  string -> [`Dir of string | `Other of other] -> [> `Unit of comp_unit]
(** [unit name ~dir ~available ~flags ~deps] is a compilation unit
    named [name] (the filename without extension) present in directory [dir].
    It is only available whenever [available] is true,
    it must be build with [flags] and depends on [deps] to be built. *)

val other : ?available:Features.t -> ?flags:Flags.t ->
  ?deps:component list -> ?action:Action.t -> string ->
  [`C | `Ml | `Mli] list -> [> `Other of other]
(** Generated OCaml source file(s). The custom action get the name of
    the build dir as argument. *)

val c : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
    ?dir:string -> ?link_flags:string list -> string -> string list ->
  [> `C of c]
(** [c name deps libs] is the C file [name.c], which need the C
    libraries [libs] to be compiled -- and it has the dependencies
    [deps]. *)

val lib : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?pack:bool -> ?c:[`C of c] list -> string -> [`Unit of comp_unit] list ->
  [> `Lib of lib]
(** [lib name units] is the project library [name] composed by the compilation
    units [cus]. If [lib] is set, use [ocamldep] to approximate the
    compilation units and their dependecies in the given directory. *)

val lib_pp : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?pack:bool -> ?c:[`C of c] list -> string -> [`Unit of comp_unit] list ->
  [> `Lib of lib]
(** [lib_pp] is like {!lib} but it defines a project pre-processor. *)

val bin : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
    ?byte_only:bool -> ?link_all:bool -> ?install:bool ->
  string -> [`Unit of comp_unit] list -> [> `Bin of bin]
(** [bin name units] is the binary [name] obtained by compiling
    the compilation units [units], with the dependencies [deps]. By
    default, the source files are located into {i bin/} (this is
    controled by the value of [dir]). *)

val files : ?available:As_features.t -> ?flags:As_flags.t ->
  ?deps:component list -> ?install:bool ->
  [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
  | `Misc | `Stublibs | `Man | `Other of string ] -> component list ->
  [> `Files of files ]
(** [dir name ~available ~flags ~deps contents] is a directory named
    [name] that contains the build artefacts of the component [contents].
    If [install] is [true] (default), the artifacts are installed in the
    corresponding directory under the install prefix. It is only available
    whenever [available] is true, it must be build with [flags] and
    depends on [deps] and [contents] to be built. *)

val js : [`Bin of bin] -> string list -> [> `JS of js]
(** [js bin args] is the decription of a javascript artefact generated
    by [js_of_ocaml]. *)

val pkg : ?available:Features.t -> ?flags:Flags.t -> ?opt:bool -> string ->
  [> `Pkg of pkg]
(** [pkg available opt name] is an external OCaml package named [name]. It is
    only available whenever [available] is true. If [opt] is true (defaults
    to [false]) a feature [f] is automatically created for the package
    and anded to [available]. *)

val pkg_pp : ?available:Features.t -> ?flags:Flags.t -> ?opt:bool -> string ->
  [> `Pkg of pkg]
(** [pkg_pp available opt name] is like {!pkg} except it denotes
    an external OCaml pre-processor package. *)

val pkg_c : ?available:Features.t -> ?flags:Flags.t -> ?opt:bool -> string ->
  [> `Pkg of pkg ]
(** [pkg_c available opt name] is like {!pkg} except it denotes an
    external C package. *)

type test_command
(** The type for test commands. *)

type test_args = (component -> string) -> string list
(** The type for command line arguments when calling tests of executable. *)

val test_bin : [`Bin of bin] -> ?args:test_args -> unit -> test_command
(** A test which runs a binary built in the project. *)

val test_shell : ('a, unit, string, test_command) format4 -> 'a
(** A test which runs an arbitrary shell command. *)

val test : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?dir:string -> string -> test_command list -> [> `Test of test]
(** Description of a test. *)

(** {1:componenthelpers Component helpers} *)

val ocamldep :
  ?keep:(string -> bool) ->
  ?deps:(string -> component list) ->
  ?unit:(string -> component list -> [ `Unit of comp_unit]) ->
  dir:string -> unit -> [> `Unit of comp_unit] list
(** [ocamldep ~dir ~keep ~deps ~unit ()] is the list of compilation
    units derived as follows.

    First the set of compilation unit names is derived by looking for
    any ml and mli files in [dir]. This set is then filtered by
    keeping only the unit names that satisfy the [keep] predicate
    (defaults to [fun _ -> true]).

    For each found compilation name [n] a first set of dependencies is
    determined by calling [deps n] (e.g. to specify packages and
    pre-processors). ocamldep is then invoked and the resulting
    compilation units are constructed by [unit n deps'] where [deps']
    is the union of deps found by ocamldep and [deps n] ([unit]
    defaults to [fun n deps' -> unit ~dir n deps']). *)

val cstubs : ?available:Features.t -> ?deps:component list ->
  ?headers:string list -> ?cflags:string list -> ?clibs:string list ->
  string -> [`Dir of string] -> [> `Lib of lib]
(** [stubs name dir] is the C stub generations, using Ctypes, of the
    compilation unit [name]. The [Name_bindings] module should be
    located in [dir]. *)

(** {1:projects Projects} *)

type t
(** The type for OCaml projects descriptions. Simply a set of
    components. *)

val create :
  ?available:Features.t ->
  ?flags:Flags.t ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?doc_public:string list ->
  ?version:string ->
  string -> component list -> t
(** [create name deps] registers the project named [name], defining
    the libraries, binaries and tests defined by the transitive
    closure of objects in [deps]. *)

val add : t -> unit
(** [add t] registers the project [t] for use by the assemblage
    command line tools. *)

(** {1:buildenv Build Environments} *)

module Build_env: sig
  (** Global project environment.

      The build environment (which can be an human) discovers available
      features. *)

  type t
  (** Environment values. *)

  val default : t
  (** Default project configuration. *)

  val parse : ?doc:string -> ?man:string list -> string -> Features.set -> t
  (** [parse name features] parse the arguments given on the
      command line as a configuration value, for the project [name] with
      the possible features [features]. *)

end

(** {1:tools Tools} *)

type tool = t -> Build_env.t -> unit
(** The signature of tools. *)

val process : ?file:string -> string -> tool -> unit
(** [process ~file name fn] reads and processes the OCaml [file] in a
    top-level environment (the default is [assemble.ml]), for the
    project called [name], and apply [fn] to the projects registered
    as side-effects. *)

val configure : [`Make] -> tool
(** Configure the project by generating the build, META and .install
    files, using the given build system backend (currently, only GNU
    make is supported). *)

val describe : tool
(** Describe the project to stdout. *)
