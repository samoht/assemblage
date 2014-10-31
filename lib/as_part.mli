(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Daniel C. Bünzli
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

(** Project parts.

    See {!Assemblage.Part}. *)

(** {1 Parts} *)

type kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo | `Custom ]

val kind_to_string : kind -> string

type part_kind = kind

type +'a t constraint 'a = [< kind]

(** {1 Coercions} *)

val coerce : ([< kind] as 'b) -> 'a t -> 'b t
val coerce_if : ([< kind] as 'b) -> 'a t -> 'b t option

(** {1 Base fields} *)

val name : 'a t -> string
val kind : 'a t -> kind
val cond : 'a t -> bool As_conf.value
val args : 'a t -> As_args.t
val deps : 'a t -> kind t list
val actions : 'a t -> As_action.t list

(** {1 Derived fields} *)

val products : 'a t -> As_path.rel list As_conf.value
(* TODO val active : 'a t -> bool As_conf.value  *)

(** {1 Comparing} *)

val id : 'a t -> string
val equal : 'a t -> 'b t -> bool
val compare : 'a t -> 'b t -> int

(** {1 Part list operations} *)

val keep : ('a t -> bool) -> 'a t list -> 'a t list
val keep_kind : (kind as 'b) -> 'a t list -> 'b t list
val keep_kinds : kind list -> 'a t list -> 'a t list
val keep_map : ('a t -> 'b option) -> 'a t list -> 'b list
val to_set : 'a t list -> 'a t list

(** {1 Specific parts} *)

module Base : sig
  val create :
    ?cond:bool As_conf.value -> ?args:(kind t -> As_args.t) ->
    ?deps:'a t list -> string ->
    (kind t -> As_action.t list) -> [> `Base] t
end

module Unit : sig
  type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
  type ocaml_unit = [ `Ml | `Mli | `Both ]
  type c_unit = [ `C | `H | `Both ]

  type kind = [ `OCaml of ocaml_unit * ocaml_interface | `C of c_unit | `Js ]

  val kind : [< `Unit] t -> kind
  val src_dir : [< `Unit] t -> As_path.rel

  val create :
    ?cond:bool As_conf.value -> ?args:As_args.t -> ?deps:'a t list ->
    ?src_dir:(As_path.rel) -> string -> kind -> [> `Unit] t

  val check_set : 'a t list -> 'a t list

  val of_base : src_dir:(As_path.rel) -> kind -> [`Base] t ->
    [> `Unit] t

  val ocaml : 'a t -> [> `Unit] t option
  val c : 'a t -> [> `Unit] t option
  val js : 'a t -> [> `Unit] t option
end

module Lib : sig
  type kind = [ `OCaml | `OCaml_pp | `C ]

  val kind : [< `Lib] t -> kind
  val byte : [< `Lib] t -> bool
  val native : [< `Lib] t -> bool
  val native_dynlink : [< `Lib] t -> bool

  val create :
    ?cond:bool As_conf.value -> ?args:As_args.t -> ?deps:part_kind t list ->
    ?byte:bool -> ?native:bool -> ?native_dynlink:bool ->
    string -> kind -> [< `Unit] t list -> [> `Lib] t

  val of_base :
    ?byte:bool -> ?native:bool -> ?native_dynlink:bool -> kind -> [`Base] t ->
    [> `Lib] t

  val ocaml : 'a t -> [> `Lib] t option
  val ocaml_pp : 'a t -> [> `Lib] t option
  val c : 'a t -> [> `Lib] t option
end

module Bin : sig
  type kind = [ `OCaml | `OCaml_toplevel | `C ]

  val kind : [< `Bin] t -> kind
  val byte : [< `Bin] t -> bool
  val native : [< `Bin] t -> bool
  val js : [< `Bin] t -> bool

  val create :
    ?cond:bool As_conf.value ->
    ?args:As_args.t ->
    ?deps:part_kind t list ->
    ?byte:bool -> ?native:bool -> ?js:bool ->
    string -> kind -> [< `Unit] t list -> [> `Bin] t

  val of_base : ?byte:bool -> ?native:bool -> ?js:bool -> kind ->
    [< `Base] t -> [> `Bin] t

(*
  val cmd : ?args:As_args.t -> ?kind:[`Byte | `Native] -> [< `Bin] t ->
    (string list -> string list) -> As_action.cmd
*)

  val ocaml : 'a t -> [> `Bin] t option
  val ocaml_toplevel : 'a t -> [> `Bin] t option
  val c : 'a t -> [> `Bin] t option
end

module Pkg : sig
  type kind = [ `OCaml | `C ]

  val kind : [< `Pkg] t -> kind

  type ocaml_lookup = [ `OCamlfind ]
  type c_lookup = [ `Pkg_config ]
  type spec = [ `C of c_lookup | `OCaml of ocaml_lookup ]

  val create :
    ?cond:bool As_conf.value -> ?args:As_args.t -> string -> spec -> [> `Pkg] t

  val of_base : kind -> [< `Base] t -> [> `Pkg] t

  val ocaml : 'a t -> [> `Pkg] t option
  val c : 'a t -> [> `Pkg] t option
end

module Run : sig
  val run_dir : [< `Run] t -> As_path.t

  val create :
    ?cond:bool As_conf.value ->
    ?args:As_args.t ->
    ?deps:'a t list ->
    ?run_dir:As_path.t ->
    string -> As_action.t -> [> `Run] t

  val of_base : ?run_dir:As_path.t -> [< `Base] t -> [> `Run] t
end

module Doc : sig
  type kind = [ `OCamldoc ]
  val kind : [< `Doc] t -> [`OCamldoc ]

  val create :
    ?cond:bool As_conf.value ->
    ?args:As_args.t ->
    ?deps:'a t list ->
    ?keep:([< `Unit] t -> bool) ->
    ?kind:kind -> string -> 'a t list -> [> `Doc] t

  val of_base : ?kind:kind -> [< `Base] t -> [> `Doc ] t

  val default : [< `Unit] t -> bool
  val dev : [< `Unit] t -> bool
end

module Dir : sig
  type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
              | `Etc | `Doc | `Misc | `Stublibs | `Man | `Other of string ]

  val kind : [< `Dir] t -> kind
  val install : [< `Dir] t -> bool

  val create :
    ?cond:bool As_conf.value ->
    ?args:As_args.t ->
    ?deps: 'a t list ->
    ?keep:('a t -> As_path.t list) ->
    ?install:bool -> kind -> 'a t list -> [> `Dir ] t

  val of_base : ?install:bool -> [> `Base] t -> [> `Dir] t

  val default : 'a t -> As_path.t list
end

module Silo : sig
  val create :
    ?cond:bool As_conf.value ->
    ?args:As_args.t ->
    ?deps:'a t list ->
    string -> 'a t list -> [> `Silo] t

  val of_base : [< `Base] t -> [> `Silo] t
end

module Custom : sig
  type data

  val key : unit -> ('a -> data) * (data -> 'a option)
  val data : [< `Custom] t -> data
  val of_base : data -> [< `Base ] t -> [> `Custom] t
end
