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

(** EDSL to describe OCaml projects. *)

module Feature: sig

  (** Project features. *)

  type t
  (** Feature values. *)

  val name: t -> string
  (** The feature name. *)

  val doc: t -> string
  (** The feature documentation. *)

  val default: t -> bool
  (** Default value. *)

  val create: doc:string -> default:bool -> string -> t
  (** Create a feature. *)

  val parse: t -> (t * bool) Cmdliner.Term.t
  (** A cmldiner term which parses a feature. *)

  val native: t
  (** Is native-code enabled ? *)

  val native_dynlink: t
  (** Is dynlink for native code enabled ? *)

end

module rec Dep: sig

  (** Library dependencies. *)

  type t
  (** Dependency values. *)

  (** {2 Compilation units} *)

  val unit: Unit.t -> t
  (** A compilation unit in the same project. *)

  val units: Unit.t list -> t list
  (** A list of compilation units. *)

  val get_units: t list -> Unit.t list
  (** Return the list of compilation unit in the dependency list. *)

  val map_units: (Unit.t -> Unit.t) -> t list -> t list
  (** Map a function over the compilation units. *)

  (** {2 Libraries} *)

  val lib: Lib.t -> t
  (** A local library. *)

  val libs: Lib.t list -> t list
  (** A list of local libraries. *)

  val get_libs: t list -> Lib.t list
  (** Return the list of local libraries in the dependency list. *)

  val pkg: string -> t
  (** A globally installed package. *)

  val pkgs: string list -> t list
  (** A list of globally installed packages. *)

  val get_pkgs: t list -> string list
  (** Return the list of globally installed packages in the dependency
      list. *)

  (** {2 Pre-processors} *)

  val p4o: Lib.t -> t
  (** A local syntax extension, using [camlp4o]. *)

  val p4os: Lib.t list -> t list
  (** A set of local syntax extensions, using [camlp4o]. *)

  val get_p4os: t list -> Lib.t list
  (** Return the local extensions in the depency list. *)

  val pkg_p4o: string -> t
  (** A globally installed syntax extension. *)

  val pkg_p4os: string list -> t list
  (** A list of globally installed syntax extensions. *)

  val get_pkg_p4os: t list -> string list
  (** Return the list of globally installed syntax extension in the
      dependency list. *)

  (** {2 Custom generation rules} *)

  type custom = {
    inputs : string list;
    outputs: string list;
    recipe : string list
  }
  (** Custom rules. *)

  val custom: custom -> t
  (** Custom generation rule. *)

  val get_customs: t list -> custom list
  (** Return the list of custom rules. *)

  (** {2 Misc} *)

  type resolver = [`Buildir of string | `Pkgs of string list] -> string
  (** Resolve a list of package names into a list of command-line
      arguments. *)

  val closure: t list -> t list
  (** Compute the transitive closure of dependencies. Try to keep the
      order as consistent as possible. *)

end

and Unit: sig

  (** Compilation unit. *)

  type t

  val copy: t -> t
  (** Copy the compilation unit. *)

  val name: t -> string
  (** Return the compilation unit name. *)

  val dir: t -> string option
  (** Return the compilation unit directory. *)

  val deps: t -> Dep.t list
  (** Return the compilation unit dependencies. *)

  val lib: t -> Lib.t option
  (** Return the library the compilation unit belongs to. *)

  val build_dir: t -> string option
  (** Return the build directory. Usually it is the library name, but
      can be other exotic places (for instance the exec name, if the
      compilation unit is the main program file). *)

  val for_pack: t -> string option
  (** Return the (optional) pack the compilation unit is in. *)

  val add_deps: t -> Dep.t list -> unit
  (** Add more dependencies to the compilation unit. *)

  val create: ?dir:string -> ?deps:Dep.t list -> string -> t
  (** Create a compilation unit. *)

  val pack: t list -> string -> t
  (** Pack a collection of compilation units together. *)

  val unpack: t -> t list
  (** Return the (usually empty) list of packed compilation units. *)

  val cmi: t -> string
  (** Return the name of the compiled module interface. *)

  val cmo: t -> string
  (** Return the name of the compiled module object. *)

  val cmx: t -> string
  (** Return the name of the extra information for native linking of
      the compilation unit. *)

  val o: t -> string
  (** Return the name of the object file for the compilation unit. *)

  val file: t -> string -> string
  (** [file t ext] is the generated file with the extension [ext] for
       the compilation unit [t]. *)

  val generated_files: t -> (Feature.t list * string) list
  (** Return the list of generated files when the given conjonction of
      features are enable. *)

  val compflags: t -> Dep.resolver -> string list
  (** Return the computed compilation flags. The [resolver] function
      resolves a list of external library names into a list of command-line
      options for the compilers. *)

  val p4oflags: t -> Dep.resolver -> string list
  (** Return the computed pre-processing flags. The [resolver] function
      resolves a list of external syntax extensions into a list of
      command-line options for the linkers. *)

  val prereqs: t -> [`byte | `native] -> string list
  (** [prereqs t mode] is the list of prerequisites files to build, in
      the given [mode], before building the compilation unit [t]. *)

end

and Lib: sig

  (** Libraries. *)

  type t

  val name: t -> string
  (** Return the library name. *)

  val filename: t -> string
  (** Return the library filename. Usually, it is the same as [name],
      but this could be updated when the library is put in a named
      project to [project.name]. *)

  val units: t -> Unit.t list
  (** Return the list of compilation units which defines the
      library. *)

  val features: t -> Feature.t list
  (** Return the features which enables the build of that library. *)

  val create:
    ?features:Feature.t list ->
    ?pack:bool ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  (** Create a library. *)

  val cma: t -> string
  (** Return the bytecode archive of the library. *)

  val cmxa: t -> string
  (** Return the extra information about the native archive of the
      library. *)

  val a: t -> string
  (** Return the native archive of the libray. *)

  val cmxs: t -> string
  (** Return the shared archive of the library. *)

  val file: t -> string -> string
  (** Return the generated file with the given extension for the given
      compilation unit. *)

  val generated_files: t -> (Feature.t list * string) list
  (** Return the list of generated files for the given project
      configuration. *)

  val deps: t -> Dep.t list
  (** Return the list of dependencies of the compilation units in the
      library. *)

end

module Top: sig

  (** Build toplevels. *)

  type t

  val name: t -> string
  (** Return the toplevel name. *)

  val deps: t -> Dep.t list
  (** Return the dependencies linked by the toplevel. *)

  val features: t -> Feature.t list
  (** Return the features which enables the build of that toplevel. *)

  val custom: t -> bool
  (** Should the toplevel be compiled with the [custom] option ? *)

  val create:
    ?features:Feature.t list ->
    ?custom:bool ->
    ?deps:Dep.t list ->
    string -> t
  (** Create a custom toplevel from a set of libraries. *)

  val byte: t -> string
  (** Return the name of the bytecode toplevel. *)

  val generated_files: t -> (Feature.t list * string) list
  (** Return the list of generated files for the given project
      configuration. *)

end

module Bin: sig

  (** Build binaries. *)

  type t

  val name: t -> string
  (** Return the binary name. *)

  val deps: t -> Dep.t list
  (** Return the dependencies linked by the binary. *)

  val create:
    ?features:Feature.t list ->
    ?deps:Dep.t list ->
    string -> t
  (** Build a binary by linking a set of libraries and additional
      compilation units. *)

  val byte: t -> string
  (** Return the name of the byte-code binary. *)

  val native: t -> string
  (** Return the name of the native binary. *)

  val generated_files: t -> (Feature.t list * string) list
  (** Return the list of generated files for the given project
      configuration. *)

end

type t
(** Project values. *)

val name: t -> string
(** Return the project name. *)

val version: t -> string
(** Return the project version. *)

val libs: t -> Lib.t list
(** Return the list of libraries defined by the project. *)

val p4os: t -> Lib.t list
(** [p4os t] is the list of syntax extensions defined by the project
    [t]. *)

val bins: t -> Bin.t list
(** Return the list of binaries defined by the project. *)

val tops: t -> Top.t list
(** Return the list of toplevels defined by the project. *)

val create:
  ?libs:Lib.t list ->
  ?p4os:Lib.t list ->
  ?bins:Bin.t list ->
  ?tops:Top.t list ->
  ?version:string ->
  string -> t
(** [create ?libs ?p4os ?bins ?tops ?version name] is the project
    description named [name], defining the libraries [libs], the
    syntax extensions [p4os], the program binaries [bins] and the
    top-level binaries [bins]. *)

val features: t -> Feature.t list
(** Return the features used in the project. *)
