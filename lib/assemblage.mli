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
    to describe [OCaml] {{!projects}projects}. It also provides simple
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

  val byte: t
  (** [byte] is true iff byte code compilation is available. *)

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

  val doc : t
  (** [public_doc] is true iff the documentation must be built. *)

end

(** Flags

    Flags values denote sets of partial command line arguments given
    to tools in a given context. A context is defined by a {{!phase}phase}
    and a {{!mode}mode}.

    FIXME describe valid context and to what they correspond. *)
module Flags : sig

  (** {1:flags Flags} *)

  type phase =
    [ `Prepare
    | `Dep
    | `Pp of [`Byte|`Native]
    | `Compile of [`Intf|`Byte|`Native|`C|`Js]
    | `Archive of [`Byte|`Native|`Shared|`C]
    | `Link of [`Byte|`Native|`Js]
    | `Run of [`Byte|`Native]
    | `Test
    | `Doc
    | `Other of string ]
  (** The type for compilation phases. *)

  type args = string list
  (** The type for partial command line arguments. *)

  type t
  (** The type for multi-context, partial, command line arguments. *)

  val v : ?available:Features.t -> phase -> args -> t
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

  val build_dir : t -> string
  (** [build_dir t] is the directory where lives all the build
      artifacts. *)

  val pkgs : t -> string list -> Flags.t
  (** Resolve global package names into command line flags. *)

end

(** {1:actions Custom Actions} *)

module Action: sig

  (** Actions to generate source files. *)

  type action
  (** Custom actions. *)

  type file =
    [ `Dep of [`Ml|`Mli]
    | `Ml | `Mli | `C | `Js
    | `Cmt | `Cmti
    | `Cmi | `Cmo | `Cmx | `O
    | `So | `Cma | `Cmxa | `Cmxs
    | `A | `Byte | `Native
    | `Dir
    | `Source of file
    | `Ext of string
    | `Other of (string -> string) ]
  (** The different kinds of files. *)

  val create : ?dir:string -> ('a, unit, string, action) format4 -> 'a
  (** [bash ~dir fmt] is a generator which produces some results by
      calling [fmt] in a bash shell, running in the directory
      [dir]. *)

  type 'a t = 'a -> Resolver.t -> Flags.t -> action
  (** Type of action generators. *)

  type 'a node =
    [ `Self of file
    | `Phony of string
    | `N of 'a * file ]
  (** The type of nodes in the action graph. *)

  type 'a rule = {
    phase  : Flags.phase;
    targets: 'a node list;
    prereqs: 'a node list;
    action : 'a t;
  }
  (** An action rule is a list of target nodes, prerequesite nodes and
      an action. *)

  val rule:
    phase:Flags.phase ->
    targets:'a node list ->
    prereqs:'a node list ->
    'a t -> 'a rule
  (** Create a new rule. *)

  val empty: 'a t
  (** The generator of empty actions. *)

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

type pkg
(** The type for package descriptions. *)

type lib
(** The type for library descriptions. *)

type bin
(** The type for binary executable descriptions. *)

type dir
(** The type for directory of file artifacts descriptions. *)

type test
(** The type for test descriptions. *)

type doc
(** The type for documentation descriptions. *)

type component =
  [ `Unit of comp_unit
  | `Other of other
  | `Lib of lib
  | `Pkg of pkg
  | `Bin of bin
  | `Dir of dir
  | `Test of test
  | `Doc of doc ]
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
(** [unit name dir ~available ~flags ~deps] is a compilation unit
    named [name] (the filename without extension) present in directory
    [dir].  It is only available whenever [available] is true, it must
    be build with [flags] and depends on [deps] to be built. *)

val pack: ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  string -> [`Unit of comp_unit] list -> [> `Unit of comp_unit]
(** Pack compilation units together. *)

val c : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?cclib: string list -> ?ccopt: string list ->
  string -> [`Dir of string | `Other of other] -> [> `Unit of comp_unit]
(** Same as {!unit} but for C source files. *)

val js : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?jsflags: string list ->
  string -> [`Dir of string | `Other of other] -> [> `Unit of comp_unit]
(** Same as {!unit} but for javascript source files. *)

val other : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  string -> component Action.rule list -> [> `Other of other]
(** Generated source file(s). *)

val lib : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?pack:bool ->
  string -> [`Units of [`Unit of comp_unit] list | `Other of other]
  -> [> `Lib of lib]
(** [lib name units] is the project library [name] composed by the compilation
    units [cus]. If [lib] is set, use [ocamldep] to approximate the
    compilation units and their dependecies in the given directory. *)

val lib_pp : ?available:Features.t -> ?flags:Flags.t ->
  ?deps:component list -> ?pack:bool ->
  string -> [`Units of [`Unit of comp_unit] list | `Other of other]
  -> [> `Lib of lib]
(** [lib_pp] is like {!lib} but it defines a project pre-processor. *)

val bin : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?byte:bool -> ?native:bool -> ?js:bool ->
  ?link_all:bool -> ?install:bool ->
  string -> [`Units of [`Unit of comp_unit] list | `Other of other]
  -> [> `Bin of bin]
(** [bin name units] is the binary [name] obtained by compiling
    the compilation units [units], with the dependencies [deps]. By
    default, the source files are located into {i bin/} (this is
    controled by the value of [dir]). *)

val dir : ?available:Features.t -> ?flags:Flags.t ->
  ?deps:component list ->
  ?install:bool ->
  [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
  | `Misc | `Stublibs | `Man | `Other of string ] -> component list ->
  [> `Dir of dir ]
(** [dir name ~available ~flags ~deps contents] is a directory named
    [name] that contains the build artefacts of the component [contents].
    If [install] is [true] (default), the artifacts are installed in the
    corresponding directory under the install prefix. It is only available
    whenever [available] is true, it must be build with [flags] and
    depends on [deps] and [contents] to be built. *)

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

type test_args = Resolver.t -> string list
(** The type for command line arguments when calling tests of executable. *)

val test_bin : [`Bin of bin] -> ?args:test_args -> unit -> test_command
(** A test which runs a binary built in the project. *)

val test_shell : ('a, unit, string, test_command) format4 -> 'a
(** A test which runs an arbitrary shell command. *)

val test : ?available:Features.t -> ?flags:Flags.t -> ?deps:component list ->
  ?dir:string -> string -> test_command list -> [> `Test of test]
(** Description of a test. *)

val doc : ?available:Features.t -> ?flags:Flags.t ->
  ?deps:component list -> ?install:bool ->
  string -> component list -> [> `Doc of doc]
(** Description of documentation files. *)

(** {1:componenthelpers Component helpers} *)

val pick: string -> component -> component
(** [unit_in c name] is the unit named [name] is the component
    [c]. Raise [Not_found] if not unit has this name. *)

val build_dir: component -> Resolver.t -> string
(** [build_dir t r] is the directory where the component [t] is
    built. *)

val root_dir: Resolver.t -> string
(** [root_dir r] is the root directory of the project. *)

val cstubs : ?available:Features.t -> ?deps:component list ->
  ?headers:string list -> ?cflags:string list -> ?clibs:string list ->
  string -> [`Dir of string] -> [> `Lib of lib]
(** [stubs name dir] is the C stub generations, using Ctypes, of the
    compilation unit [name]. The [Name_bindings] module should be
    located in [dir]. *)

(** {1:projects Projects} *)

type project
(** The type for OCaml projects descriptions. Simply a set of
    components. *)

val project :
  ?available:Features.t ->
  ?flags:Flags.t ->
  ?version:string ->
  string -> component list -> project
(** [project name comps]  TODO *)

(** {1:buildenv Build Environments} *)

module Build_env: sig
  (** Build environment.

      The build environment (which can be an human) discovers available
      features. *)

  type t
  (** Environment values. *)

  val default : t
  (** Default project configuration. *)
end

(** {1:commands Commands} *)

val assemble : project -> unit
(** [assemble project] runs the default assemblage command line
    tool with the assemble file [file] (defaults to [assemble.ml]. *)

val (/): string -> string -> string
(** Same as [Filename.concat]. *)
