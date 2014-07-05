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

type gen
(** Generated source code. *)

type dep =
  [ `Comp of comp
  | `Lib of lib
  | `Pp of lib
  | `Pkg_pp of string
  | `Pkg of string
  | `Bin of bin
  | `C of c
  | `JS of js
  | `Test of test
  | `Gen of gen ]
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

val generated: ?deps:dep list -> ?action:(Resolver.t -> Action.t) ->
  [`Both|`ML|`MLI] -> string -> dep
(** Generated OCaml source file(s). *)

val c: ?dir:string -> ?link_flags:string list -> string list -> string -> dep
(** [c libs name] is the C file [name.c], which need the C libraries
    [libs] to be compiled. *)

val cstubs:
  ?bag:string -> ?dir:string -> ?headers:string list ->
  ?cflags:string list -> ?clibs:string list ->
  dep list -> string -> dep
(** [stubs deps name] is the C stub generations, using Ctypes, of the
    compilation unit [name]. *)

val lib: ?bag:string -> string -> dep
(** Build a library from a bag of compilation units. By default, use
    all the compilation unit registered in the {i "main"} bag. *)

val bin:
  ?byte_only:bool -> ?link_all:bool -> ?install:bool -> ?dir:string ->
  dep list -> string list -> string -> dep
(** [bin deps units name] is the binary [name] obtained by compiling
    the compilation units [units], with the dependencies [deps]. By
    default, the source files are located into {i bin/} (this is
    controled by the value of [dir]). *)

val js: bin -> string list -> dep
(** [js bin args] is the decription of a javascript artefact generated
    by [js_of_ocaml]. *)

val pkg: string -> dep
(** An external package. *)

val pkg_pp: string -> dep
(** An external pre-processor. *)

val create:
  ?flags:Flags.t ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?version:string ->
  dep list -> string -> unit
(** [create deps name] registers the project named [name], defining
    the libraries, binaries and tests defined by the transitive
    closure of objects in [deps]. *)

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

val contents: t -> dep list
(** Return the project contents. *)

val list: unit -> t list
(** Return the project already registered. *)

val generated_from_custom_generators: t -> Resolver.t -> string list
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

module Gen: sig

  (** Source file generator. *)

  include S with type t = gen

  val create: ?deps:dep list -> ?action:(Resolver.t -> Action.t) ->
    [`Both|`ML|`MLI]-> string -> t
  (** Generate source files, using the given action. *)

  val copy: t -> t
  (** Copy the generator if it needs to run in an other directory. *)

end

module Comp: sig

  (** Signature for compilation units. *)

  include S with type t = comp

  val create:
    ?flags:Flags.t ->
    ?dir:string ->
    ?deps:dep list -> string -> t
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

  val generated: t -> bool
  (** [generator t] is either [None], which means that the source file
      is not generated, or [Some files] when the source files [files]
      of the compilation unit are generated. *)

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
    ?deps:dep list -> comp list -> string -> t
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

  val create: ?dir:string -> ?generated:bool -> ?link_flags:string list ->
    ?deps:dep list -> string -> t
  (** Create a C object file. *)

  val dll_so: t -> Resolver.t -> string
  (** The location of the generated [.so] file. *)

end

module Test: sig

  (** Test-cases *)

  include S with type t = test
  (** Test values. *)

  type command =
    [ `Bin of bin * string list
    | `Shell of string ]

  val create: ?dir:string -> ?deps:dep list -> command list -> string -> t
  (** Create a test. *)

  val dir: t -> string option
  (** The directory where to run the test. *)

  val commands: t -> command list
  (** The list of commands to run the test. *)

end

module Dep: sig

  (** Manage dependencies. *)

  type t = dep

  module Graph: G with type V.t = dep
  (** The dependency graph. *)

  val comp: t -> comp option
  (** Is the dependency a compilation unit? *)

  val lib: t -> lib option
  (** Is the dependency a local library? *)

  val pkg: t -> string option
  (** Is the dependency a globally installed library in a package? *)

  val pp: t -> lib option
  (** Is the dependency a local syntax extension. *)

  val pkg_pp: t -> string option
  (** Is the dependency a globally installed syntax extension. *)

  val bin: t -> bin option
  (** Is the dependency a binary. *)

  val gen: t -> gen option
(** Is the dependency a generated source file. *)

  val js: t -> js option
  (** Is the dependency a js_of_ocaml binary? *)

  val test: t -> test option
  (** Is the dependency a test? *)

  val filter: (dep -> 'a option) -> dep list -> 'a list
  (** Filter a list of dependencies. *)

  (** {2 Dependency closure} *)

  val closure: t list -> t list
  (** Compute the transitive closure of dependencies. Try to keep the
      order as consistent as possible. *)

end
