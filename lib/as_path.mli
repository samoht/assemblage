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
type segs = private string list
type rel = [`Rel of segs]
type abs = [`Abs of segs]
type t = [ abs | rel ]

val current : [> rel]
val root : [> abs]
val is_current : [> rel] -> bool
val is_root : [> abs] -> bool
val is_rel : [> rel] -> bool
val is_abs : [> abs] -> bool
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
val to_string : [< t ] -> string
val of_string : string -> t

(** {1 File system queries} *)

val exists : [< t ] -> bool
val is_file : [< t ] -> bool
val is_dir : [< t ] -> bool

(** {1 File extensions} *)

type ext =
  [ `Ml_dep | `Mli_dep | `Ml | `Mli | `C | `H | `Js | `Cmi | `Cmo | `Cmx | `O
  | `Cmt | `Cmti | `Cma | `Cmxa | `Cmxs | `A | `So | `Byte | `Native
  | `Ext of string ]

val ext_to_string : ext -> string
val ext_of_string : string -> ext
val has_ext : ext -> [< t ] -> bool
val ext : [< t ] -> ext option
val get_ext : [< t ] -> ext
val add_ext : [< t ] -> ext -> t
val chop_ext : [< t ] -> t
val ( + ) : [< t ] -> ext -> t
