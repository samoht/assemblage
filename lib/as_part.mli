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

type meta

val meta_key : unit -> ('a -> meta) * (meta -> 'a option)

type kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo | `Custom ]

val kind_to_string : kind -> string

type +'a t =
  { kind : kind;
    name : string;
    cond : bool As_conf.value;
    deps : kind t list;
    args : kind t -> As_args.t;
    actions : kind t -> As_action.t list;
    meta : meta; }
constraint 'a = [< kind ]

val create : ?cond:bool As_conf.value -> ?args:(kind t -> As_args.t) ->
  ?deps:'a t list -> ?actions:(kind t -> As_action.t list) -> string ->
  ([< kind] as 'b) -> meta -> 'b t

val name : 'a t -> string
val kind : 'a t -> kind
val cond : 'a t -> bool As_conf.value
val args : 'a t -> As_args.t
val deps : 'a t -> kind t list
val actions : 'a t -> As_action.t list
val meta : 'a t -> meta
val get_meta : (meta -> 'a option) -> 'b t -> 'a

(** {1 Derived fields} *)

val products : 'a t -> As_path.rel list As_conf.value
(* TODO val active : 'a t -> bool As_conf.value  *)

(** {1 Coercions} *)

val coerce : ([< kind] as 'b) -> 'a t -> 'b t
val coerce_if : ([< kind] as 'b) -> 'a t -> 'b t option

(** {1 Part list operations} *)

val keep : ('a t -> bool) -> 'a t list -> 'a t list
val keep_kind : (kind as 'b) -> 'a t list -> 'b t list
val keep_kinds : kind list -> 'a t list -> 'a t list
val keep_map : ('a t -> 'b option) -> 'a t list -> 'b list
val to_set : 'a t list -> 'a t list

(** {1 Comparing} *)

val id : 'a t -> string
val equal : 'a t -> 'b t -> bool
val compare : 'a t -> 'b t -> int
