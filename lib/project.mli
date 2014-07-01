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

module Flags: sig

  (** Command line arguments (ie. flags). *)

  type t
  (** Command-line arguments. *)

  type f = string list -> string list
  (** Command-line transformers. *)

  val create:
    ?comp_byte:f -> ?comp_native:f ->
    ?pp_byte:f -> ?pp_native:f  ->
    ?link_byte:f -> ?link_native:f ->
    unit -> t
  (** Create a basic command-line argument with the given
      transformers. *)

  val (@): t -> t -> t
  (** Append command-line flags. *)

  val comp_byte: t -> f
  (** The command-line arguments for compiling compilation units in
      bytecode mode. *)

  val comp_native: t -> f
  (** The command-line arguments for compiling compilation units in
      native mode. *)

  val pp_byte: t -> f
  (** The command-line arguments for pre-processing files in bytecode
      mode. *)

  val pp_native: t -> f
  (** The command-line arguments for pre-processing files in native
      mode. *)

  val link_byte: t -> f
  (** The command-line arguments to link compilation units in bytecode
      mode. *)

  val link_native: t -> f
  (** The command-line arguments to link compilation units in native
      mode. *)

  (** {2 Built-in flags} *)

  val empty: t
  (** Empty flags. *)

  val debug: t
  (** Add [-g]. *)

  val annot: t
  (** Add [-bin-annot]. *)

  val warn_error: t
  (** Add [-warn-error]. *)

end

module Feature: sig

  (** Project features. *)

  type t
  (** Feature values. *)

  type formula
  (** Feature formulaes. *)

  type cnf = [ `Conflict | `And of [ `P of t | `N of t ] list ]
  (** Conjonctive Normal Form. *)

  val (@): cnf -> cnf -> cnf
  (** Concatenation of CNF formulaes. *)

  val normalize: formula -> cnf
  (** [atoms f] is the list of atoms appearing in the formula [f]. *)

  val eval: (t * bool) list -> formula -> bool
  (** [eval tbl f] evaluates the formula [f] given the truth table
      [tbl]. If a feature [t] does not appear in [tbl] is is
      considered as associated to [false]. *)

  val true_: formula
  (** The formula which is always [true]. *)

  val false_: formula
  (** The formula which is always [false]. *)

  val atom: t -> formula
  (** [atom t] is the formula containing the singleton feature [t]. *)

  val not: formula -> formula
  (** [not f] negates the formula [f]. *)

  val (&&): formula -> formula -> formula
  (** [f1 && f2] is the conjonction of [f1] and [f2]. *)

  val (||): formula -> formula -> formula
  (** [f1 || f2] is the disjonction of [f1] and [f2]. *)

  val name: t -> string
  (** The feature name. *)

  val default: t -> bool
  (** Default value. *)

  val with_default: t -> bool -> t
  (** Return the feature with an other default. *)

  val create: doc:string -> default:bool -> string -> t
  (** Create a feature. *)

  val parse: t -> (t * bool) Cmdliner.Term.t
  (** A cmldiner term which parses a feature. *)

  val native: formula
  val native_t: t
  (** Is native-code enabled ? *)

  val native_dynlink: formula
  val native_dynlink_t: t
  (** Is dynlink for native code enabled ? *)

  val annot: formula
  val annot_t: t
  (** Generate annot files ? *)

  val debug: formula
  val debug_t: t
  (** Generate debug symbols ? *)

  val warn_error: formula
  val warn_error_t: t
  (** Consider warning as error. *)

  val test: formula
  val test_t: t
  (** Compile and run tests. *)

  val doc: formula
  val doc_t: t
  (** Build the documentation. *)


  module Set: Set.S with type elt = t
  (** Set of features. *)

  val base: Set.t
  (** The base features. *)

end

module Resolver: sig

  (** Dependency resolver. *)

  type t
  (** Resolver values. *)

  val create: buildir:(string -> string) -> pkgs:(string list -> Flags.t) -> t
  (** [create ~buildir ~pkgs] is the resolver which apply the function
      [buildir] to resolve local libraries and resolves a set of
      global package by applying [pkgs]. *)

  val build_dir: t -> string -> string
  (** Resolve locally generated filename (by usually prepending the
      build directory name). *)

  val pkgs: t -> string list -> Flags.t
  (** Resolve global package names into command-line flag
      transformers. *)

end

module Gen: sig

  (** Generated source file. *)

  type t
  (** Generator values. *)

  val func: (unit -> unit) -> t
  (** [func fn] is a generator which produces some results by calling
      [fn ()]. *)

  val shell: dir:string -> string -> string list -> t
  (** [shell ~dir cmd args] is a generator which produces some results
      by calling [cmd args] in a shell running in the directory
      [dir]. *)

  val bash: dir:string -> ('a, unit, string, t) format4 -> 'a
  (** [bash ~dir fmt] is a generator which produces some results by
      calling [fmt] in a bash shell, running in the directory
      [dir]. *)

  val run: t -> unit
  (** Run the generator. *)

end

module rec Dep: sig

  (** Library dependencies. *)

  type t =
    [ `Unit of Unit.t
    | `Lib of Lib.t
    | `Pp of Lib.t
    | `Pkg_pp of string
    | `Pkg of string
    | `Bin of Bin.t ]
  (** Dependency values. *)

  val id: t -> string
  (** Unique name of the dependency. *)

  (** {2 Compilation units} *)

  val unit: Unit.t -> t
  (** A compilation unit in the same project. *)

  val units: Unit.t list -> t list
  (** A list of compilation units. *)

  val filter_units: t list -> Unit.t list
  (** [filter_units deps] is the list of compilation unit in
      [deps]. *)

  (** {2 Libraries} *)

  val lib: Lib.t -> t
  (** A local library. *)

  val libs: Lib.t list -> t list
  (** A list of local libraries. *)

  val filter_libs: t list -> Lib.t list
  (** [filter_libs deps] is the list of local libraries in [deps]. *)

  val pkg: string -> t
  (** A globally installed library in a package. *)

  val pkgs: string list -> t list
  (** A list of globally installed libraries in packages. *)

  val filter_pkgs: t list -> string list
  (** [filter_pkgs deps] is the list of globally installed libraries in
      packages contained in [deps]. *)

  (** {2 Binaries} *)

  val bin: Bin.t -> t
  (** A local binary. *)

  val bins: Bin.t list -> t list
  (** A list of local binaries. *)

  (** {2 Pre-processors} *)

  val pp: Lib.t -> t
  (** A local syntax extension. *)

  val pps: Lib.t list -> t list
  (** A set of local syntax extensions. *)

  val filter_pps: t list -> Lib.t list
  (** [filter_pps deps] is the list of local extensions in [deps]. *)

  val pkg_pp: string -> t
  (** A globally installed syntax extension. *)

  val pkg_pps: string list -> t list
  (** A list of globally installed syntax extensions. *)

  val filter_pkg_pps: t list -> string list
  (** [filter_pkg_pps] is the list of globally installed syntax
      extension in [deps]. *)

  (** {2 Dependency closure} *)

  val closure: t list -> t list
  (** Compute the transitive closure of dependencies. Try to keep the
      order as consistent as possible. *)

end

and Unit: sig

  (** Compilation unit. *)

  type t
  (** Description values for compilation units. *)

  val create:
    ?flags:Flags.t ->
    ?generator:(Gen.t * [`Both|`ML|`MLI]) ->
    ?dir:string ->
    ?deps:Dep.t list ->
    string -> t
  (** Create a compilation unit. *)

  val id: t -> string
  (** Unique name of the compilation unit. This is the composition of
      the [build_path] and the unit [name]. *)

  val copy: t -> t
  (** Copy the compilation unit. *)

  val name: t -> string
  (** The name of the compilation unit. *)

  val dir: t -> string option
  (** The source directory of the compilation unit. *)

  val deps: t -> Dep.t list
  (** The dependencies of the compilation unit. *)

  val container: t -> [`Lib of Lib.t |`Bin of Bin.t]  option
  (** The library the compilation unit belongs to. *)

  val mli: t -> bool
  (** Has the compilation unit an [mli] file. *)

  val ml: t -> bool

  (** Has the compilation unit an [ml] file. *)
  val for_pack: t -> string option
  (** The (optional) pack the compilation unit is in. *)

  val generator: t -> (Gen.t * [`Both|`ML|`MLI]) option
  (** [generated t] is either [None], which means that the source file
      is not generated, or [Some (gen, kind)] when [gen] is the
      generator of the source file and [kind ] is the kind of file
      which is generated. *)

  val pack: ?flags:Flags.t -> t list -> string -> t
  (** Pack a collection of compilation units together. *)

  val unpack: t -> t list
  (** The (usually empty) list of packed compilation units. *)

  val cmi: t -> Resolver.t -> string
  (** The location of the generated compiled module interface. *)

  val cmo: t -> Resolver.t -> string
  (** The location of the generated compiled module object. *)

  val cmx: t -> Resolver.t -> string
  (** The location of the extra information for native linking of the
      compilation unit. *)

  val o: t -> Resolver.t -> string
  (** The location of the object file for the compilation unit. *)

  val file: t -> Resolver.t -> string -> string
  (** [file t ext] is the generated file with the extension [ext] for
       the compilation unit [t]. *)

  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  (** [generated_files t r] is the list of generated files when the
      given formula of features are enable. *)

  val flags: t -> Resolver.t -> Flags.t
  (** [flags t r] is the computed compilation flags, where [r]
      resolves a dependency names into a list of command-line
      flags. *)

  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  (** [prereqs t resolver mode] is the list of prerequisites files to
      build, in the given [mode], before building the compilation unit
      [t], where [resolver] is used to compute the location of
      generated files. *)

  val build_dir: t -> Resolver.t -> string
  (** The build directory of the compilation unit. *)

end

and Lib: sig

  (** Libraries. *)

  type t
  (** Description values for libraries. *)

  val create:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?pack:bool ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  (** Create a library. *)

  val id: t -> string
  (** Unique name of the library (to avoid name clashes with binaries
      having the same name). *)

  val name: t -> string
  (** The library name. *)

  val filename: t -> string
  (** The library filename. Usually, it is the same as [name], but
      this could be updated when the library is put in a named project
      to [project.name]. *)

  val units: t -> Unit.t list
  (** The list of compilation units which defines the library. *)

  val available: t -> Feature.formula
  (** The features which enables the build of that library. *)

  val deps: t -> Dep.t list
  (** The list of dependencies of the compilation units in the
      library. *)

  val cma: t -> Resolver.t -> string
  (** The location of the generated bytecode archive. *)

  val cmxa: t -> Resolver.t -> string
  (** The location of the extra information about the native
      archive. *)

  val a: t -> Resolver.t -> string
  (** The location of the native archive. *)

  val cmxs: t -> Resolver.t -> string
  (** The location of the shared archive. *)

  val file: t -> Resolver.t -> string -> string
  (** [file t ext] is the generated file with the extension [ext] for
       the library [t]. *)

  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  (** [generated_files t r] is the list of generated files when the
      given formula of features are enable. *)

  val flags: t -> Resolver.t -> Flags.t
  (** [flags t r] is the computed compilation flags, where [r]
      resolves a dependency names into a list of command-line
      flags. *)

  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  (** [prereqs t resolver mode] is the list of prerequisites files to
      build, in the given [mode], before building the compilation unit
      [t], where [resolver] is used to compute the location of
      generated files. *)

  val build_dir: t -> Resolver.t -> string
  (** The build directory of the library. *)

end

and Bin: sig

  (** Build binaries. *)

  type t
  (** Description values for binaries. *)

  val create:
    ?available:Feature.formula ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    ?flags:Flags.t ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  (** Build a binary by linking a set of compilation units. *)

  val toplevel:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?custom:bool ->
    ?install:bool ->
    ?deps:Dep.t list ->
    Unit.t list -> string -> t
  (** Create a custom toplevel by linking a set of compilation
      units. *)

  val id: t -> string
  (** Unique name of the binary (to avoid name-clashes with library
      having the same name). *)

  val name: t -> string
  (** The binary name. *)

  val units: t -> Unit.t list
  (** The list of compilation units contained in the binary. *)

  val deps: t -> Dep.t list
  (** The dependencies linked by the binary. *)

  val available: t -> Feature.formula
  (** The features which enables the build of that library. *)

  val is_toplevel: t -> bool
  (** Is the binary a toplevel. *)

  val install: t -> bool
  (** Should the binary be installed. *)

  val byte: t -> Resolver.t -> string
  (** The location of the generated byte-code binary. *)

  val native: t -> Resolver.t -> string
  (** The location of the generated native binary. *)

  val generated_files: t -> Resolver.t -> (Feature.formula * string list) list
  (** [generated_files t r] is the list of generated files when the
      given formula of features are enable. *)

  val flags: t -> Resolver.t -> Flags.t
  (** [flags t r] is the computed compilation flags, where [r]
      resolves a dependency names into a list of command-line
      flags. *)

  val prereqs: t -> Resolver.t -> [`Byte | `Native] -> string list
  (** [prereqs t resolver mode] is the list of prerequisites files to
      build, in the given [mode], before building the compilation unit
      [t], where [resolver] is used to compute the location of
      generated files. *)

  val build_dir: t -> Resolver.t -> string
  (** The build directory of the binary. *)

end

module Test: sig

  (** Test-cases *)

  type t
  (** Test values. *)

  val create: ?dir:string -> Bin.t -> string list -> string -> t
  (** Create a test. *)

  val bin: t -> Bin.t
  (** The binary name. *)

  val dir: t -> string option
  (** The directory where to run the test. *)

  val args: t -> string list
  (** The arguments to pass to the test program. *)

  val id: t -> string
  (** The test unique name. *)

end

type t
(** Project values. *)

val name: t -> string
(** Return the project name. *)

val version: t -> string
(** Return the project version. *)

val libs: t -> Lib.t list
(** Return the list of libraries defined by the project. *)

val pps: t -> Lib.t list
(** [pps t] is the list of syntax extensions defined by the project
    [t]. *)

val bins: t -> Bin.t list
(** [bins t] is the list of binaries defined by the project. *)

val tests: t -> Test.t list
(** [tests t] is the list of tests in the project. *)

val create:
  ?flags:Flags.t ->
  ?libs:Lib.t list ->
  ?pps:Lib.t list ->
  ?bins:Bin.t list ->
  ?tests:Test.t list ->
  ?css:string ->
  ?intro:string ->
  ?doc_dir:string ->
  ?version:string ->
  string -> unit
(** [create ?libs ?pps ?bins ?version name] registers the project
    named [name], defining the libraries [libs], the syntax extensions
    [pps] and the program binaries [bins]. *)

val list: unit -> t list
(** Return the project lists. *)

val features: t -> Feature.Set.t
(** Return the features used by the project. *)

val css: t -> string option
(** The name of the CSS file for the project documentation. *)

val intro: t -> string option
(** The name of the intro file for the project documentation. *)

val doc_dir: t -> string
(** Return the directory where the HTML documentation is generated. *)

val generators: t -> Resolver.t -> string list
(** Return the list of generated file from a custom generator. *)

module Graph: sig

  (** Graph of values. *)

  module type S = sig

    include Graph.Sig.I

    val iter: (V.t -> unit) -> t -> unit
    (** Topoligical iteration. *)

    val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Topological fold. *)

    val vertex: t -> V.t list
    (** [vertex g] is the list of topologically sorted vertices. *)

  end

  module Dep: S with type V.t = Dep.t
  (** Graph of dependencies. *)

  module Unit: S with type V.t = Unit.t
  (** Graph of compilation units. *)

  module Lib: S with type V.t = Lib.t
  (** Graph of libraries. *)

  module Bin: S with type V.t = Bin.t
  (** Graph of binaries. *)

end

val unit: ?units:Graph.Unit.t -> ?dir:string -> Dep.t list -> string -> Dep.t
(** Register a compilation unit. *)

val lib: ?units:Graph.Unit.t -> string -> Lib.t
(** Build a library from a graph of compilation units. *)

val pkg: string -> Dep.t
(** An external package. *)

val pkg_pp: string -> Dep.t
(** An external pre-processor. *)
