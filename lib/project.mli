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

module Flag: sig

  (** Conditional flags. *)

  type t
  (** Flag values. *)

  val name: t -> string
  (** The flag name. *)

  val doc: t -> string
  (** The flag documentation. *)

  val create: doc:string -> string -> t
  (** Create a flag. *)

  val parse: t -> (t * bool option) Cmdliner.Term.t
  (** A cmldiner term which parses a flag. *)

end

module Conf: sig

  (** Global project configuration. *)
  type t

  val create:

    ?native:bool ->
    (** Enable native compilation. Default is [true]. *)

    ?native_dynlink: bool ->
    (** Enable compilation of native dynlink units. Default is
        [true]. *)

    ?flags:(Flag.t * bool) list ->
    (** Environment flags. *)

    ?comp: string list ->
    (** Additional compilation flags passed to both [ocamlc] and
        [ocamlopt]. *)

    ?bytcomp: string list ->
    (** Additional compilation flags passed to [ocamlc] only. *)

    ?natcomp: string list ->
    (** Additional compilation flags passed to [ocamlopt] only. *)

    ?link: string list ->
    (** Additional link flags passed to both [ocamlc] and [ocamlopt]. *)

    ?bytlink: string list ->
    (** Addtional link flags passed to [ocamlc]. *)

    ?natlink: string list ->
    (** Addtional link flags passed to [ocamlopt]. *)

    ?p4o: string list ->
    (** Addition flags passed to the camlp4o preprocessor. *)

    ?destdir: string ->
    (** Location of the generated files. [None] means the files stays in
        the same directory. *)

    unit -> t

  val default: t
  (** Default project configuration. *)

  val parse: Flag.t list -> t Cmdliner.Term.t
  (** Parse the arguments given on the command-line as a configuration
      value. *)

  val native: t -> bool
  (** Check if the native compilers are enable in the given
      configuration. *)

  val native_dynlink: t -> bool
  (** Check if the native dynlinker is enable in the given
      configuration. *)

  val comp: t -> string list
  (** Return the global comand-line flags for compilation. *)

  val link: t -> string list
  (** Return the global command-line flags for linking. *)

  val p4o: t -> string list
  (** Return the global command-line option for the camlp4o preprocessor. *)

  val destdir: t -> string
  (** Return the directory where build artififacts are generated. *)

  val enable: t -> Flag.t list -> bool
  (** Check if the given set of flags are all enabled. *)

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

  type resolver = string list -> string list
  (** Resolve a list of package names into a list of command-line
      arguments. *)

  val closure: t list -> t list
  (** Compute the transitive closure of dependencies. Try to keep the
      order as consistent as possible. *)

end

and Unit: sig

  (** Compilation unit. *)

  type t

  val name: t -> string
  (** Return the compilation unit name. *)

  val dir: t -> string option
  (** Return the compilation unit directory. *)

  val deps: t -> Dep.t list
  (** Return the compilation unit dependencies. *)

  val lib: t -> Lib.t option
  (** Return the library the compilation unit belongs to. *)

  val flags: t -> Flag.t list
  (** Return the compilation unit conditional flags. *)

  val add_deps: t -> Dep.t list -> t
  (** Add more dependencies to the compilation unit. *)

  val with_lib: t -> Lib.t -> t
  (** Set the library of the compilation unit. *)

  val create: ?dir:string -> ?deps:Dep.t list -> ?flags:Flag.t list -> string -> t
  (** Create a compilation unit. *)

  val generated_files: t -> Conf.t -> string list
  (** Return the list of generated files for the given project
      configuration. *)

  val compflags: t -> Conf.t -> Dep.resolver -> string list
  (** Return the computed compilation flags. The [resolver] function
      resolves a list of external library names into a list of command-line
      options for the compilers. *)

  val p4oflags: t -> Conf.t -> Dep.resolver -> string list
  (** Return the computed pre-processing flags. The [resolver] function
      resolves a list of external syntax extensions into a list of
      command-line options for the linkers. *)

end

and Lib: sig

  (** Libraries. *)

  type t

  val name: t -> string
  (** Return the library name. *)

  val units: t -> Unit.t list
  (** Return the list of compilation units. *)

  val create: ?flags:Flag.t list -> ?deps:Dep.t list -> Unit.t list -> string -> t
  (** Create a library. *)

  val generated_files: t -> Conf.t -> string list
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

  val libs: t -> Lib.t list
  (** Return the libraries linked by the toplevel. *)

  val custom: t -> bool
  (** Should the toplevel be compiled with the [custom] option ? *)

  val create: ?custom:bool -> Lib.t list -> string -> t
  (** Create a custom toplevel from a set of libraries. *)

  val generated_files: t -> Conf.t -> string list
  (** Return the list of generated files for the given project
      configuration. *)

end

module Bin: sig

  (** Build binaries. *)

  type t

  val name: t -> string
  (** Return the binary name. *)

  val libs: t -> Lib.t list
  (** Return the libraries linked by the binary. *)

  val units: t -> Unit.t list
  (** Return the compilation units linked by the binary. *)

  val create: Lib.t list -> Unit.t list -> string -> t
  (** Build a binary by linking a set of libraries and additional
      compilation units. *)

  val generated_files: t -> Conf.t -> string list
  (** Return the list of generated files for the given project
      configuration. *)

end

type t
(** Project values. *)

val name: t -> string option
(** Return the project name. *)

val version: t -> string option
(** Return the project version. *)

val libs: t -> Lib.t list
(** Return the list of libraries defined by the project. *)

val bins: t -> Bin.t list
(** Return the list of binaries defined by the project. *)

val tops: t -> Top.t list
(** Return the list of toplevels defined by the project. *)

val conf: t -> Conf.t
(** Return the project configuration. *)

val with_name: t -> string -> t
(** Set the project name. *)

val with_version: t -> string -> t
(** Set the project version. *)

val create:
  ?name:string ->
  ?version:string ->
  ?flags:Flag.t list ->
  ?conf:Conf.t ->
  ?libs:Lib.t list ->
  ?bins:Bin.t list ->
  ?tops:Top.t list ->
  unit -> t
(** Generate a project description for the given collection of
    libraries, with the given project settings. If [conf] is not set,
    use the given [flags] to read the settings from the command line
    arguments. *)
