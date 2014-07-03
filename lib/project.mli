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

(** {2 Basic types} *)

type t
(** Description of full OCaml projects. *)

type comp
(** Description of compilation units. *)

type lib
(** Description of libraries. *)

type bin
(** Description of binaries. *)

type test
(** Description of tests. *)

type js
(** Description of js_of_ocaml build artifacts. *)

type c
(** C stubs. *)

type dep =
  [ `Comp of comp
  | `Lib of lib
  | `Pp of lib
  | `Pkg_pp of string
  | `Pkg of string
  | `Bin of bin
  | `C of c ]
(** Values representing the dependencies. [`Lib] are local libraries,
    [`Pp] are local pre-processors. [`Pkg] are globally installed
    packages (managed using {i ocamlfind}, [`Pkg_pp] are globally
    installed pre-processor packages. [`Bin] are local binaries. *)

(** {2 Basic API} *)

val comp: ?bag:string -> ?dir:string -> dep list -> string -> dep
(** [comp ~lib ~dir deps name] is the compilation unit in the bag of
    compilation units [bag], located in the directory [dir], which the
    dependencies [deps] and the cname [name]. The name is the same as
    the filename, without its extension.  By default, [dir] is set to
    {i lib/} and [bag] is {i "main"}. *)

val c: Flags.t -> string -> dep
(** [c flags name] is the C file [name.c], which need the flags
    [flags] to be compiled. *)

val stubs:
  ?bag:string -> ?dir:string -> ?headers:string list -> ?cflags:string list ->
  string list -> string -> dep
(** [stubs name] is the C stub generations, using Ctypes, of the
    compilation unit [name]. *)

val lib: ?bag:string -> string -> lib
(** Build a library from a bag of compilation units. By default, use
    all the compilation unit registered in the {i "main"} bag. *)

val bin:
  ?byte_only:bool -> ?link_all:bool -> ?install:bool -> ?dir:string ->
  dep list -> string list -> string -> bin
(** [bin deps units name] is the binary [name] obtained by compiling
    the compilation units [units], with the dependencies [deps]. By
    default, the source files are located into {i bin/} (this is
    controled by the value of [dir]). *)

val js: bin -> string list -> js
(** [js bin args] is the decription of a javascript artefact generated
    by [js_of_ocaml]. *)

val pkg: string -> dep
(** An external package. *)

val pkg_pp: string -> dep
(** An external pre-processor. *)

val create:
  ?flags:Flags.t ->
  ?libs:lib list -> ?pps:lib list ->
  ?bins:bin list -> ?tests:test list ->
  ?jss:js list -> ?cs:c list ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?version:string -> string -> unit
(** [create ?libs ?pps ?bins ?version name] registers the project
    named [name], defining the libraries [libs], the syntax extensions
    [pps] and the program binaries [bins]. *)

val name: t -> string
(** Return the project name. *)

val version: t -> string
(** Return the project version. *)

val features: t -> Feature.Set.t
(** Return the features used in the project. *)

val doc_css: t -> string option
(** The name of the CSS file for the project documentation. *)

val doc_intro: t -> string option
(** The name of the intro file for the project documentation. *)

val doc_dir: t -> string
(** Return the directory where the HTML documentation is generated. *)

val comps: t -> comp list
(** [unit t] is the list of compilation units in the project [t]. The
    list is sorted in compilation order. *)

val libs: t -> lib list
(** [libs t] is the list of libraries in the project [t]. The list is
    sorted in compilation order. *)

val pps: t -> lib list
(** [pps t] is the list of syntax extensions in the project [t]. The
    list is sorted in compilation order. *)

val bins: t -> bin list
(** [bins t] is the list of binaries defined by the project [t]. The
    list is sorted in compilation order. *)

val tests: t -> test list
(** [tests t] is the list of tests in the project. *)

val jss: t -> js list
(** [jss t] is the list of [js_of_ocaml] generated files in the
    project. *)

val cs: t -> c list
(** [cs t] is the list of C stubs in the project. *)

val list: unit -> t list
(** Return the project already registered. *)

val generated_from_custom_actions: t -> Resolver.t -> string list
(** Return the list of generated file from a custom action
    generators. *)

(** {2 Extended API} *)

module type G = sig

  (** Signature for graphs of managed objects. *)

  include Graph.Sig.I

  val iter: (V.t -> unit) -> t -> unit
  (** Topoligical iteration. *)

  val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Topological fold. *)

  val vertex: t -> V.t list
  (** [vertex g] is the list of topologically sorted vertices. *)

end

module type S = sig

  (** Abstract signature shared by all objects. *)

  type t
  (** Object values. *)

  val id: t -> string
  (** Unique name of the object. *)

  val name: t -> string
  (** The name that the user gave to the object. *)

  val deps: t -> dep list
  (** The object dependencies. *)

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

  module Graph: G with type V.t = t
  (** Graph of objects. *)

end

module Comp: sig

  (** Signature for compilation units. *)

  include S with type t = comp

  val create:
    ?flags:Flags.t ->
    ?action:((Resolver.t -> Action.t) * [`Both|`ML|`MLI]) ->
    ?dir:string ->
    ?deps:dep list ->
    string -> t
  (** Create a compilation unit. *)

  val copy: t -> t
  (** Copy the compilation unit. *)

  val dir: t -> string option
  (** The source directory of the compilation unit. *)

  val container: t -> [`Lib of lib |`Bin of bin]  option
  (** The library the compilation unit belongs to. *)

  val mli: t -> bool
  (** Has the compilation unit an [mli] file. *)

  val ml: t -> bool
  (** Has the compilation unit an [ml] file. *)

  val for_pack: t -> string option
  (** The (optional) pack the compilation unit is in. *)

  val action: t -> (Action.t * [`Both|`ML|`MLI]) option
  (** [generated t] is either [None], which means that the source file
      is not generated, or [Some (gen, kind)] when [action] is the
      action to perform to generate the source file and [kind ] is the
      kind of file which is generated. *)

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

end

module Lib: sig

  (** Library descriptions. *)

  include S with type t = lib

  val create:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?pack:bool ->
    ?deps:dep list ->
    comp list -> string -> t
  (** Create a library. *)

  val filename: t -> string
  (** The library filename. Usually, it is the same as [name], but
      this could be updated when the library is put in a named project
      to [project.name]. *)

  val comps: t -> comp list
  (** The list of compilation units which defines the library. *)

  val available: t -> Feature.formula
  (** The features which enables the build of that library. *)

  val cma: t -> Resolver.t -> string
  (** The location of the generated bytecode archive. *)

  val cmxa: t -> Resolver.t -> string
  (** The location of the extra information about the native
      archive. *)

  val a: t -> Resolver.t -> string
  (** The location of the native archive. *)

  val cmxs: t -> Resolver.t -> string
  (** The location of the shared archive. *)

end

module Bin: sig

  (** Binary descriptions. *)

  include S with type t = bin

  val create:
    ?available:Feature.formula ->
    ?byte_only:bool ->
    ?link_all:bool ->
    ?install:bool ->
    ?flags:Flags.t ->
    ?deps:dep list ->
    comp list -> string -> t
  (** Build a binary by linking a set of compilation units. *)

  val toplevel:
    ?available:Feature.formula ->
    ?flags:Flags.t ->
    ?custom:bool ->
    ?install:bool ->
    ?deps:dep list ->
    comp list -> string -> t
  (** Create a custom toplevel by linking a set of compilation
      units. *)

  val comps: t -> comp list
  (** The list of compilation units contained in the binary. *)

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

end

module JS: sig

  (** Compilation to JavaScript. *)

  include S with type t = js

  val create: bin -> string list -> t
  (** Create a {i .js} object, using [js_of_ocaml]. *)

  val js: t -> Resolver.t -> string
  (** The location of the generated javascript artifacts. *)

end

module C: sig

  (** C files. *)

  include S with type t = c

  val create: ?dir:string -> ?flags:Flags.t -> string -> t
  (** Create a C object file. *)

  val dll_so: t -> Resolver.t -> string
  (** The location of the generated [.so] file. *)

end

module Test: sig

  (** Test-cases *)

  include S with type t = test
  (** Test values. *)

  val create: ?dir:string -> bin -> string list -> string -> t
  (** Create a test. *)

  val bin: t -> bin
  (** The binary name. *)

  val dir: t -> string option
  (** The directory where to run the test. *)

  val args: t -> string list
  (** The arguments to pass to the test program. *)

end

module Dep: sig

  (** Manage dependencies. *)

  type t = dep

  module Graph: G with type V.t = dep
  (** The dependency graph. *)

  (** {2 Compilation units} *)

  val comp: comp -> t
  (** A compilation unit in the same project. *)

  val comps: comp list -> t list
  (** A list of compilation units. *)

  val filter_comps: t list -> comp list
  (** [filter_comps deps] is the list of compilation unit in
      [deps]. *)

  (** {2 Libraries} *)

  val lib: lib -> t
  (** A local library. *)

  val libs: lib list -> t list
  (** A list of local libraries. *)

  val filter_libs: t list -> lib list
  (** [filter_libs deps] is the list of local libraries in [deps]. *)

  val pkg: string -> t
  (** A globally installed library in a package. *)

  val pkgs: string list -> t list
  (** A list of globally installed libraries in packages. *)

  val filter_pkgs: t list -> string list
  (** [filter_pkgs deps] is the list of globally installed libraries in
      packages contained in [deps]. *)

  (** {2 Binaries} *)

  val bin: bin -> t
  (** A local binary. *)

  val bins: bin list -> t list
  (** A list of local binaries. *)

  (** {2 Pre-processors} *)

  val pp: lib -> t
  (** A local syntax extension. *)

  val pps: lib list -> t list
  (** A set of local syntax extensions. *)

  val filter_pps: t list -> lib list
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
