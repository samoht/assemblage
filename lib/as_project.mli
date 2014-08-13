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
type other
type pkg
type lib
type bin
type dir
type test
type doc
type container

type component =
  [ `Unit of comp_unit
  | `Other of other
  | `Pkg of pkg
  | `Lib of lib
  | `Bin of bin
  | `Dir of dir
  | `Test of test
  | `Doc of doc ]

type dirname =
  [ `Lib | `Bin | `Sbin | `Toplevel
  | `Share | `Share_root | `Etc | `Doc
  | `Misc | `Stublibs | `Man | `Other of string ]

(** Common signature shared by all components. *)
module type Component_base = sig
  type t
  val id: t -> string
  val name: t -> string
  val source_dir: t -> string option
  val available: t -> As_features.t
  val flags : t -> As_resolver.t -> As_flags.t
  val deps : t -> component list
  val container : t -> container option
  val contents : t -> component list
  val rules: t -> component As_action.rule list
  val generated_files : t -> (As_features.t * As_action.file list) list
end

(** Signature for graphs of components. *)
module type Graph = sig
  include Graph.Sig.I

  val iter : (V.t -> unit) -> t -> unit
  (** Topoligical iteration. *)

  val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Topological fold. *)

  val to_list : t -> V.t list
  val of_list : V.t list -> t
end

(** Rules. *)
module Rule: sig
  val files: component -> As_resolver.t -> component As_action.node list -> string list
  val phony_run: component -> string
end

(** Signature for sets of components. *)
module type Set = sig
  include Set.S
  val to_list : t -> elt list
  val of_list : elt list -> t
end

(** Component descriptions. *)
module Component: sig
  include Component_base with type t = component
  val unit : t -> comp_unit option
  val unit_ocaml : t -> comp_unit option
  val unit_c : t -> comp_unit option
  val unit_js : t -> comp_unit option
  val other : t -> other option
  val pkg : t -> pkg option
  val pkg_ocaml : t -> pkg option
  val pkg_ocaml_pp : t -> pkg option
  val pkg_c : t -> pkg option
  val lib : t -> lib option
  val lib_ocaml : t -> lib option
  val lib_ocaml_pp : t -> lib option
  val bin : t -> bin option
  val dir : t -> dir option
  val test : t -> test option
  val doc : t -> doc option
  val filter : (t -> 'a option) -> t list -> 'a list
  val closure : ?link:bool -> component list -> component list
  val build_dir: t -> As_resolver.t -> string
  val file : t -> As_resolver.t -> As_action.file -> string
  val source: t -> As_action.file -> string
  val map: (t -> t) -> t list -> t list
  val phases: t -> As_flags.phase list
  module Set: Set with type elt = t
  module Graph: Graph with type V.t = t
end

(** Containers *)
module Container: sig
  type t = container
  val name: t -> string
  val id: all:bool -> t -> string
  val available: all:bool -> t -> As_features.t
  val flags: all:bool -> t -> As_flags.t
  val deps: all:bool -> t -> component list
end

(** Compilation units. *)
module Unit : sig
  include Component_base with type t = comp_unit
  type kind = [ `OCaml | `C | `Js ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list ->
    string -> kind -> [`Path of string list | `Other of other] -> t
  val pack : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string -> t list -> t
  val generated: t -> bool
  val kind: t -> [`OCaml | `C | `Js]
  val has : As_action.file -> t -> bool
end

(** External package. *)
module Pkg : sig
  include Component_base with type t = pkg
  type kind = [ `OCaml | `OCaml_pp | `C ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?opt:bool -> string -> kind -> t
  val opt: t -> bool
  val kind : t -> kind
  val compiler_libs_toplevel : t
  val ctypes_stub : t
end

(** Library descriptions. *)
module Lib : sig
  include Component_base with type t = lib
  type kind = [ `OCaml | `OCaml_pp ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?pack:bool ->
    string -> kind ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val kind : t -> kind
end

(** Binary descriptions. *)
module Bin : sig
  include Component_base with type t = bin
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list ->
    ?byte:bool -> ?native:bool -> ?js:bool ->
    ?link_all:bool -> ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val toplevel : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?custom:bool -> ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val js : t -> bool
  val is_toplevel : t -> bool
  val install : t -> bool
end

(** Arbitrary files generator. *)
module Other : sig
  include Component_base with type t = other
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string ->
    component As_action.rule list -> t
  val empty: t
end

(** Directory with build artifacts *)
module Dir : sig
  include Component_base with type t = dir
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    dirname -> component list -> dir
  val dirname : t -> dirname
end

module Test : sig
  include Component_base with type t = test
  type args = As_resolver.t -> string list
  type command =
    [ `Bin of [`Bin of Bin.t] * args
    | `Shell of string ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?dir:string -> string -> command list -> t
end

module Doc : sig
  include Component_base with type t = doc
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    string -> component list -> t
end


(** {1 Projects} *)

type t
(** The type for describing projects. *)

val create : ?available:As_features.t -> ?flags:As_flags.t ->
  ?version:string -> string -> component list -> t
(** [create cs n] is the project named [n] with components [cs]. *)

val components : t -> component list
(** Return the project components in context. *)

val name : t -> string
(** [name t] is the project name. *)

val version : t -> string
(** [version t] is the project version. *)

val features : t -> As_features.Set.t
(** [features t] is the collection of features used in the project. *)
