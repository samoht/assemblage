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
type segs
type rel = [`Rel of segs]
type abs = [`Abs of segs]
type t = [ abs | rel ]

val current : [> rel]
val root : [> abs]
val dash : [> rel]
val is_current : [> rel] -> bool
val is_root : [> abs] -> bool
val is_rel : [> rel] -> bool
val is_abs : [> abs] -> bool
val is_dash : [> rel] -> bool
val as_rel : t -> rel
val as_abs : t -> abs
val as_path : [< t ] -> t
val basename : [< t ] -> string
val dirname : [< t ] -> t
val segs : [< t ] -> string list
val concat_seg : [< t ] -> string -> t
val concat : [< t ] -> rel -> t
val ( / ) : [< t ]  -> string -> t
val ( // ) : [< t ] -> rel -> t
val file : filename -> [> rel]
val dir : string -> [> rel]
val rel_of_segs : string list -> [> rel]
val abs_of_segs : string list -> [> abs]
val to_abs : ?rel_base:abs -> [< t ] -> [> abs]
val equal : t -> t -> bool
val compare : t -> t -> int
val of_string : string -> t
val to_string : [< t ] -> string
val quote : [< t ] -> string
val pp : Format.formatter -> t -> unit

(** {1 File extensions} *)

type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]

val ext_to_string : ext -> string
val ext_of_string : string -> ext
val has_ext : ext -> [< t ] -> bool
val ext_matches : ext list -> [< t ] -> bool
val ext : [< t ] -> ext option
val get_ext : [< t ] -> ext
val add_ext : [< t ] -> ext -> t
val rem_ext : [< t ] -> t
val change_ext : [< t ] -> ext -> t
val ( + ) : [< t ] -> ext -> t
val ( -+ ) : [< t ] -> ext -> t

(** {1 Path sets and maps} *)

module Set : sig
  include Set.S with type elt = t
  val of_list : elt list -> t
end

module Map : Map.S with type key = t
