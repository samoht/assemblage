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

(** Project

    A datastructure to describe projects. Models a project as a
    description of a set of components such as libraries, binaries,
    tests, etc. forming a DAG.

    Each component as its own particularities but they all extend
    the {!Component_base} signature. The module signature {{!Graph}Graph}
    models the relation between components. *)

type comp_unit
type gen
type file
type c
type js
type pkg
type lib
type bin
type dir
type test

type component =
  [ `Unit of comp_unit
  | `Gen of gen
  | `File of file
  | `C of c
  | `JS of js
  | `Pkg_pp of pkg
  | `Pkg of pkg
  | `Lib of lib
  | `Pp of lib
  | `Bin of bin
  | `Dir of dir
  | `Test of test ]

(** Common signature shared by all components. *)
module type Component_base = sig

  type t
  (** The type for describing a specific kind of project components. *)

  val id : t -> string
  (** [id t] is the unique name of the component [t]. *)

  val name : t -> string
  (** [name x] is the name that the user gave to the component
      [t]. Component of different kinds can have the same name (for
      instance a library and a binary), use [id] to get a unique
      name. *)

  val available : t -> As_features.t
  (** [available c] is true if component [c] is available. *)

  val flags : t -> As_resolver.t -> As_flags.t
  (** [flags t r] are the compilation flags used when generating the
      build artifacts for the project [t] and the name resolver
      [r].  *)

  val deps : t -> component list
  (** [deps t] is the list of dependencies of the component [t]. *)

  val build_dir : t -> As_resolver.t -> string
  (** [build_dir t] is the directory where build artifacts of the
      description [t] are built. *)

  val file : t -> As_resolver.t -> string -> string
  (** [file t r ext] is the location of the generated file with the
       extension [ext] for the component [t] and the name resolver
       [r]. *)

  val generated_files : t -> As_resolver.t -> (As_features.t * string list) list
  (** [generated_files t r] is the list of generated files and the
      feature which enable them, for the component [t] and the name
      resolver [r]. *)

  val prereqs : t -> As_resolver.t -> [`Byte | `Native | `Shared] -> string list
  (** [prereqs t resolver mode] is the list of prerequisites files to
      build, in the given [mode], before building the object [t],
      where [resolver] is used to compute the location of generated
      files. *)
end

module type Graph = sig

  (** Signature for graphs of components. *)

  include Graph.Sig.I

  val iter : (V.t -> unit) -> t -> unit
  (** Topoligical iteration. *)

  val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Topological fold. *)

  val to_list : t -> V.t list
  (** [to_list g] is the list of topologically sorted vertices. *)

  val of_list : V.t list -> t
  (** [of_list l] is the graph of components in the list [l]. *)

end

module type Set = sig

  (** Signature for sets of components. *)

  include Set.S

  val of_list : elt list -> t
  (** Create a set from a list of elements. *)

end

module Component : sig

  (** Component descriptions. *)

  include Component_base with type t = component

  val unit : t -> comp_unit option
  val file_ : t -> file option
  val gen : t -> gen option
  val c : t -> c option
  val js : t -> js option
  val pkg : t -> pkg option
  val pkg_pp : t -> pkg option
  val lib : t -> lib option
  val pp : t -> lib option
  val bin : t -> bin option
  val dir : t -> dir option
  val test : t -> test option

  val filter : (t -> 'a option) -> t list -> 'a list
  (** Filter a list of components. *)

  val closure : t list -> t list
  (** Compute the transitive closure of the component dependency
      graph. Try to keep the order as consistent as possible. *)

  val comp_byte : t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  (** [comp_byte ts r build_dir] is the list of command-line arguments
      to use for compiling a component depending of [ts] to bytecode,
      where the build artifacts are generated in [build_dir] and the
      external names are resolved by [r]. *)

  val comp_native : t list -> As_resolver.t -> (As_resolver.t -> string) ->
    string list
  (** Same as [comp_byte] but for native code. *)

  val pp_byte : t list -> As_resolver.t -> string list
  (** [pp_byte ts r] is the list of command-line arguments to use for
      pre-processing a component depending on [ts], using the name
      resolver [r]. *)

  val pp_native : t list -> As_resolver.t -> string list
  (** Same as [pp_native] but for native pre-processors. *)

  val link_byte : t list -> As_resolver.t -> comp_unit list -> string list
  (** [link_byte deps r cus] is the list of command-line arguments to
      use for linking the compilation units [cus] together, depending
      on the components [ts] and using the name resolver [r]. *)

  val link_native : t list -> As_resolver.t -> comp_unit list -> string list

  module Set: Set with type elt = t
  (** Set of components. *)

  module Graph: Graph with type V.t = t
  (** Graph of components. *)
end

(** Compilation units. *)
module Unit : sig

  include Component_base with type t = comp_unit

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?dir:string -> ?deps:Component.t list -> string -> t
  (** Create a compilation unit. *)

  val copy : t -> t
  (** Copy the compilation unit. *)

  val dir : t -> string option
  (** The source directory of the compilation unit. *)

  val container : t -> [`Lib of lib |`Bin of bin]  option
  (** The library the compilation unit belongs to. *)

  val mli : t -> bool
  (** Has the compilation unit an [mli] file. *)

  val ml : t -> bool
  (** Has the compilation unit an [ml] file. *)

  val for_pack : t -> string option
  (** The (optional) pack the compilation unit is in. *)

  val generated : t -> bool
  (** [generator t] is either [None], which means that the source file
      is not generated, or [Some files] when the source files [files]
      of the compilation unit are generated. *)

  val pack : ?available:As_features.t -> ?flags:As_flags.t -> t list ->
    string -> t
  (** Pack a collection of compilation units together. *)

  val unpack : t -> t list
  (** The (usually empty) list of packed compilation units. *)

  val cmi : t -> As_resolver.t -> string
  (** The location of the generated compiled module interface. *)

  val cmo : t -> As_resolver.t -> string
  (** The location of the generated compiled module object. *)

  val cmx : t -> As_resolver.t -> string
  (** The location of the extra information for native linking of the
      compilation unit. *)

  val o : t -> As_resolver.t -> string
  (** The location of the object file for the compilation unit. *)
end

(** Static file *)
module File : sig
  include Component_base with type t = file
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?dir:string -> string -> file
end

(** Source file generator. *)
module Gen : sig

  include Component_base with type t = gen

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:Component.t list -> ?action:As_action.t -> [`C | `Ml | `Mli] list ->
    string -> t
  (** Generate source files, using the given action. *)

  val copy : t -> t
  (** Copy the generator if it needs to run in an other directory. *)

  val files : t -> As_resolver.t -> string list
  (** The list of generated files. *)

  val actions : t -> As_resolver.t -> string list
  (** Return the list of actions to run in order to generate the files
      advertized by [files]. *)
end

module C : sig

  (** C files. *)

  include Component_base with type t = c

  val create :
    ?available:As_features.t ->
    ?flags:As_flags.t ->
    ?dir:string -> ?generated:bool -> ?link_flags:string list ->
    ?deps:Component.t list -> string -> t
  (** Create a C object file. *)

  val container : t -> [`Lib of lib |`Bin of bin]  option
  (** [container t] is the component which contains the given C
      objects. *)

  val set_lib_container : t -> lib -> unit
  (** Set the container to the given library. *)

  val set_bin_container : t -> bin -> unit
  (** Set the container to the given binary. *)

  val link_flags : t -> string list
  (** Return the C link flags. *)

  val dll_so : t -> As_resolver.t -> string
  (** The location of the generated [.so] file. *)

  val symlink_c : t -> As_resolver.t -> string
  (** The location of the symlinked C file. *)

  val o : t -> As_resolver.t -> string
  (** The location of the object file. *)
end

module JS : sig

  (** Compilation to JavaScript. *)

  include Component_base with type t = js

  val create : ?available:As_features.t -> bin -> string list -> t
  (** Create a {i .js} object, using [js_of_ocaml]. *)

  val js : t -> As_resolver.t -> string
  (** The location of the generated javascript artifacts. *)
end

(** External package (globally installed using [ocamlfind]. *)
module Pkg : sig

  include Component_base with type t = pkg

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?opt:bool -> string -> is_pp:bool -> t

  (** {1 Built-in packages} *)

  val compiler_libs_toplevel : t
  val ctypes_stub : t
end

(** Library descriptions. *)
module Lib : sig

  include Component_base with type t = lib

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?pack:bool -> ?deps:(string -> Component.t list) ->
    ?c:c list -> Unit.t list -> string -> t
  (** Create a library. *)

  val filename : t -> string
  (** The library filename. Usually, it is the same as [name], but
      this could be updated when the library is put in a named project
      to [project.name]. *)

  val units : t -> Unit.t list
  (** The list of compilation units which defines the library. *)

  val c_objects : t -> c list
  (** [c_objects t] is the list of C objects contained in the
      library. *)

  val cma : t -> As_resolver.t -> string
  (** The location of the generated bytecode archive. *)

  val cmxa : t -> As_resolver.t -> string
  (** The location of the extra information about the native
      archive. *)

  val a : t -> As_resolver.t -> string
  (** The location of the native archive. *)

  val cmxs : t -> As_resolver.t -> string
  (** The location of the shared archive. *)
end

(** Binary descriptions. *)
module Bin : sig

  include Component_base with type t = bin

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?byte_only:bool -> ?link_all:bool -> ?install:bool ->
    ?deps:(string -> Component.t list) ->
    Unit.t list -> string -> t
  (** Build a binary by linking a set of compilation units. *)

  val toplevel : ?available:As_features.t -> ?flags:As_flags.t ->
    ?custom:bool -> ?install:bool -> ?deps:(string -> Component.t list) ->
    Unit.t list -> string -> t
  (** Create a custom toplevel by linking a set of compilation
      units. *)

  val units : t -> Unit.t list
  (** The list of compilation units contained in the binary. *)

  val is_toplevel : t -> bool
  (** Is the binary a toplevel. *)

  val install : t -> bool
  (** Should the binary be installed. *)

  val byte : t -> As_resolver.t -> string
  (** The location of the generated byte-code binary. *)

  val native : t -> As_resolver.t -> string
  (** The location of the generated native binary. *)
end

(** Directory with build artifacts *)
module Dir : sig
  include Component_base with type t = dir

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root | `Etc | `Doc
    | `Misc | `Stublibs | `Man | `Other of string ] -> component list -> dir
end

module Test : sig

  (** Test-cases *)

  include Component_base with type t = test
  (** Test values. *)

  type args = (Component.t -> string) -> string list
  (** The command-line arguments when calling tests. *)

  type command =
    [ `Bin of [`Bin of Bin.t] * args
    | `Shell of string ]

  val create : ?available:As_features.t -> ?flags:As_flags.t -> ?dir:string ->
    ?deps:Component.t list -> command list -> string  -> t
  (** Create a test. *)

  val dir : t -> string option
  (** The directory where to run the test. *)

  val commands : t -> command list
  (** The list of commands to run the test. *)
end

(** {1 Projects} *)

type t
(** The type for describing projects. *)

val create : ?available:As_features.t -> ?flags:As_flags.t ->
  ?doc_css:string -> ?doc_intro:string -> ?doc_dir:string ->
  ?doc_public:string list -> ?version:string ->
  string -> Component.t list -> t
(** [create cs n] is the project named [n] with components [cs]. *)

val components : t -> Component.t list
(** Return the project components. *)

val name : t -> string
(** [name t] is the project name. *)

val version : t -> string
(** [version t] is the project version. *)

val features : t -> As_features.Set.t
(** [features t] is the collection of features used in the project. *)

val files_of_generators : t -> As_resolver.t -> string list
(** [files_of_generators t r] is the list of files generated by custom
    generators in the project [t], using the name resolver [r]. *)

val doc_css : t -> string option
(** [doc_css t] is the name of the CSS file for the project
    documentation. *)

val doc_intro : t -> string option
(** [doc_intro t] is the name of the intro file for the project
    documentation. *)

val doc_dir : t -> string
(** [doc_dir t] is the the directory where the HTML documentation is
    generated. *)

val doc_public : t -> string list
(** The public documentation. *)
