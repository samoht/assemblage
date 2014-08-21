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

(** Components *)

(** {1 Types} *)

type comp_unit
type other
type pkg
type lib
type bin
type container
type test
type doc

type kind = [ `Unit | `Pkg | `Lib | `Bin | `Container | `Test | `Doc | `Other ]
type t =
  [ `Unit of comp_unit
  | `Other of other
  | `Pkg of pkg
  | `Lib of lib
  | `Bin of bin
  | `Container of container
  | `Test of test
  | `Doc of doc ]
(** The type for components. *)

(** Signature for sets of components. *)
module type Set = sig
  include Set.S
  val to_list : t -> elt list
  val of_list : elt list -> t
end

(** {1 Components base fields} *)

val name : t -> string
val kind : t -> kind
val available : ?all:bool -> t -> As_features.t
val deps : ?all:bool -> t -> t list
val parent : t -> t option
val contents : t -> t list
val files : t -> (As_features.t * As_action.file list) list
val rules : t -> t As_action.rule list
val flags : ?all:bool -> t -> As_resolver.t -> As_flags.t
val id : ?all:bool -> t -> string
val build_dir : t -> As_resolver.t -> string
val file : t -> As_resolver.t -> As_action.file -> string
val source_dir : t -> As_action.file -> string
val phases : t -> As_flags.phase list

(** {1 Component list operations} *)

val keep : (t -> bool) -> t list -> t list
val filter_map : (t -> 'a option) -> t list -> 'a list
val map : (t -> t) -> t list -> t list
val closure : ?link:bool -> t list -> t list

(** {1 Component kinds} *)

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
val container : t -> container option
val test : t -> test option
val doc : t -> doc option

(** {1 Component sets} *)

module Set : Set with type elt = t

(** {1 Rules and specific components.} *)

type component = t

(** Rules. *)
module Rule : sig
  val files : component -> As_resolver.t -> component As_action.node list ->
    string list
  val phony_run : component -> string
end

(** Compilation units. *)
module Unit : sig
  type t = comp_unit
  type kind = [ `OCaml | `C | `Js ]
  type interface = [ `Normal | `Opaque | `Hidden ]

  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?interface:interface  ->
    string -> kind -> [`Path of string list | `Other of other] -> t

  val pack : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string -> t list -> t

  val generated : t -> bool
  val kind : t -> [`OCaml | `C | `Js]
  val has : As_action.file -> t -> bool
  val interface : t -> interface
  val source_dir : t -> string option
end

(** External package. *)
module Pkg : sig
  type t = pkg
  type kind = [ `OCaml | `OCaml_pp | `C ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?opt:bool -> string -> kind -> t
  val name : t -> string
  val opt : t -> bool
  val kind : t -> kind
  val compiler_libs_toplevel : t
  val ctypes_stub : t
end

(** Library descriptions. *)
module Lib : sig
  type t = lib
  type kind = [ `OCaml | `OCaml_pp ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?byte:bool -> ?native:bool ->
    ?native_dynlink:bool -> ?pack:bool -> string -> kind ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val kind : t -> kind
end

(** Binary descriptions. *)
module Bin : sig
  type t = bin
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list ->
    ?byte:bool -> ?native:bool -> ?js:bool ->
    ?linkall:bool -> ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val toplevel : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?custom:bool -> ?install:bool ->
    string ->
    [`Units of [`Unit of Unit.t] list | `Other of other] -> t
  val units: t -> comp_unit list
  val js : t -> bool
  val is_toplevel : t -> bool
  val install : t -> bool
end

(** Arbitrary files generator. *)
module Other : sig
  type t = other
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string ->
    component As_action.rule list -> t
end

(** Directory of components. *)
module Container : sig
  type t = container
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> string -> component list -> container
end

module Test : sig
  type t = test
  type args = As_resolver.t -> string list
  type command =
    [ `Bin of [`Bin of Bin.t] * args
    | `Shell of string ]
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?dir:string -> string -> command list -> t
end

module Doc : sig
  type t = doc
  val create : ?available:As_features.t -> ?flags:As_flags.t ->
    ?deps:component list -> ?install:bool ->
    string -> component list -> t
end
