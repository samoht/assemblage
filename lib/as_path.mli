(*
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

(** File paths.

    For documentation see {!Assemblage.Path}. *)

(** {1 File paths} *)

type filename = string
type rel
type abs
type t

val root : t
val empty : t
val dash : t
val add : t -> string -> t
val concat : t -> rel -> t
val ( / ) : t -> string -> t
val ( // ) : t -> rel -> t
val file : filename -> t
val base : string -> t
val basename : t -> string
val dirname :  t -> t
val rem_prefix : t -> t -> rel option
val find_prefix : t -> t -> t option

(** {1 Predicates and comparison} *)

val is_root : t -> bool
val is_empty : t -> bool
val is_dash : t -> bool
val is_rel : t -> bool
val is_abs : t -> bool
val is_prefix : t -> t -> bool
val equal : t -> t -> bool
val compare : t  -> t -> int

(** {1 Conversions} *)

val to_rel : t -> rel option
val of_rel : rel -> t
val to_abs : t -> abs option
val of_abs : abs -> t
val to_segs : t -> [ `Abs of string list | `Rel of string list ]
val of_segs : [ `Abs of string list | `Rel of string list ] -> t
val to_string : t -> string
val of_string : string -> t
val quote : t -> string
val pp : Format.formatter -> t -> unit

(** {1 File extensions} *)

type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip | `Prepare
  | `Ext of string ]

val ext_to_string : ext -> string
val ext_of_string : string -> ext
val pp_ext : Format.formatter -> ext -> unit
val ext : t -> ext option
val get_ext : t -> ext
val add_ext : t -> ext -> t
val rem_ext : t -> t
val change_ext : t -> ext -> t
val ( + ) : t -> ext -> t
val has_ext : ext -> t -> bool
val ext_matches : ext list -> t -> bool

(** {1 Relative paths} *)

(** Relative paths. *)
module Rel : sig

  (** {1 Relative paths} *)

  type path = t
  type t = rel

  val empty : rel
  val dash : rel
  val add : rel -> string -> rel
  val concat : rel -> rel -> rel
  val file : filename -> rel
  val base : string -> rel
  val ( / ) : rel -> string -> rel
  val ( // ) : rel -> rel -> rel
  val basename : rel -> string
  val dirname :  rel -> rel
  val rem_prefix : rel -> rel -> rel option
  val find_prefix : rel -> rel -> rel

  (** {1 Predicates and comparison} *)

  val is_empty : rel -> bool
  val is_dash : rel -> bool
  val is_prefix : rel -> rel -> bool
  val equal : rel -> rel -> bool
  val compare : rel  -> rel -> int

  (** {1 Conversions} *)

  val to_segs : rel -> string list
  val of_segs : string list -> rel
  val to_string : rel -> string
  val quote : rel -> string
  val pp : Format.formatter -> rel -> unit

  (** {1 File extensions} *)

  val ext : rel -> ext option
  val get_ext : rel -> ext
  val add_ext : rel -> ext -> rel
  val rem_ext : rel -> rel
  val change_ext : rel -> ext -> rel
  val ( + ) : rel -> ext -> rel
  val has_ext : ext -> rel -> bool
  val ext_matches : ext list -> rel -> bool

  (** {1 Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = rel
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = rel
    val dom : 'a t -> Set.t
  end
end

(** {1 Absolute paths} *)

(** Absolute paths. *)
module Abs : sig

  (** {1 Absolute paths} *)

  type path = t
  type t = abs
  val root : abs
  val add : abs -> string -> abs
  val concat : abs -> rel -> abs
  val ( / ) : abs -> string -> abs
  val ( // ) : abs -> rel -> abs
  val basename : abs -> string
  val dirname :  abs -> abs
  val rem_prefix : abs -> abs -> rel option
  val find_prefix : abs -> abs -> abs

  (** {1 Predicates and comparison} *)

  val is_root : abs -> bool
  val is_prefix : abs -> abs -> bool
  val equal : abs -> abs -> bool
  val compare : abs  -> abs -> int

  (** {1 Conversions} *)

  val to_segs : abs -> string list
  val of_segs : string list -> abs
  val to_string : abs -> string
  val quote : abs -> string
  val pp : Format.formatter -> abs -> unit

  (** {1 File extensions} *)

  val ext : abs -> ext option
  val get_ext : abs -> ext
  val add_ext : abs -> ext -> abs
  val rem_ext : abs -> abs
  val change_ext : abs -> ext -> abs
  val ( + ) : abs -> ext -> abs
  val has_ext : ext -> abs -> bool
  val ext_matches : ext list -> abs -> bool

  (** {1 Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = abs
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = abs
    val dom : 'a t -> Set.t
  end
end

(** {1 Path sets and maps} *)

module Set : sig
  include Set.S with type elt = t
  val of_list : elt list -> t
end

module Map : sig
  include Map.S with type key = t
  val dom : 'a t -> Set.t
end
