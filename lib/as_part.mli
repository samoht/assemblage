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

(** {1 Part kinds} *)

type kind = [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo ]
val pp_kind : Format.formatter -> kind -> unit

(** {1 Usage} *)

type usage = [ `Dev | `Test | `Build | `Doc | `Outcome | `Other of string ]
val pp_usage : Format.formatter -> usage -> unit

(** {1 Metadata} *)

type meta
val meta_key : unit -> ('a -> meta) * (meta -> 'a option)
val meta_nil : meta

(** {1 Parts} *)

type +'a t constraint 'a = [< kind ]

val v_kind : ?usage:usage -> ?cond:bool As_conf.value -> ?meta:meta ->
  ?needs:'a t list -> ?args:(kind t -> As_args.t) ->
  ?actions:(kind t -> As_action.t list) -> ?check:(kind t -> bool) ->
  string -> ([< kind] as 'b) -> 'b t

val v : ?usage:usage -> ?cond:bool As_conf.value -> ?meta:meta ->
  ?needs:'a t list -> ?args:(kind t -> As_args.t) ->
  ?actions:(kind t -> As_action.t list) ->
  ?check:(kind t -> bool) -> string -> [> `Base] t

val kind : 'a t -> kind
val name : 'a t -> string
val usage : 'a t -> usage
val cond : 'a t -> bool As_conf.value
val meta : 'a t -> meta
val get_meta : (meta -> 'a option) -> 'b t -> 'a
val needs : 'a t -> kind t list
val args : 'a t -> As_args.t
val actions : 'a t -> As_action.t list
val check : 'a t -> bool
val products : 'a t -> As_path.rel list As_conf.value
val id : 'a t -> int
val sid : 'a t -> string
val equal : 'a t -> 'b t -> bool
val compare : 'a t -> 'b t -> int
val with_kind_meta : ([< kind] as 'b) -> meta -> 'a t -> 'b t
val deps : 'a t -> As_conf.Key.Set.t

(** {1 Coercions} *)

val coerce : ([< kind] as 'b) -> 'a t -> 'b t
val coerce_if : ([< kind] as 'b) -> 'a t -> 'b t option

(** {1 Part lists} *)

val uniq : 'a t list -> 'a t list
val keep : ('a t -> bool) -> 'a t list -> 'a t list
val keep_kind : ([< kind] as 'b) -> 'a t list -> 'b t list
val keep_kinds : kind list -> 'a t list -> 'a t list
val keep_map : ('a t -> 'b option) -> 'a t list -> 'b list
val fold_rec : ('a -> kind t -> 'a) -> 'a -> kind t list -> 'a

(** {1 Part sets and maps} *)

module Set : sig
  include Set.S with type elt = kind t
  val of_list : elt list -> t
end

module Map : Map.S with type key = kind t
