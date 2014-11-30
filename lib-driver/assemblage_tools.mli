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

(** Driver project tools. *)

open Assemblage
open Assemblage.Private

(** {1 Project tools} *)

(** OPAM support.

    Synchronize OPAM metadata and generate install files. *)
module Opam : sig

  (** {1 Metadata synchronization} *)

  (** Metadata synchronization. *)
  module Sync : sig
  end

  (** {1 Install files} *)

  (** Install files.

      Generate OPAM {{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
        package installation files}. *)
  module Install : sig

    (** {1 Install file} *)

    type move
    (** The type for file moves. *)

    val move : ?maybe:bool -> ?dst:Path.t -> Path.t -> move
    (** [move src ~dst] moves [src] to [dst]. [src] is expressed
        relative to the install file and [dst] relative to the
        destination directory which is determined by the move's
        {{!field_elt}field}. If [dst] is absent [src] is placed at the root
        of the destination directory. [maybe] indicates that [src]
        may be absent (defaults to [false]). *)

    type field_elt =
      [ `Bin of move | `Doc of move | `Etc of move | `Lib of move | `Man of move
      | `Misc of move | `Sbin of move | `Share of move | `Share_root of move
      | `Stublibs of move | `Toplevel of move ]
    (** The type for field elements. Determines the destination
        directory of an {!move}. *)

    type t = [ `Header of string option ] * field_elt list
    (** The type for install files. An optional introductory comment followed
        by file moves. *)

    val to_string : t -> string
    (** [to_string i] is [i] as a string. *)

    val of_project : ?add:field_elt list -> Assemblage.project -> t
    (** [of_project add p] is an install file for project [p] and [add]. *)
  end
end

(** Findlib META support.

    Generate findlib
    {{:http://projects.camlcity.org/projects/dl/findlib-1.5.5/doc/ref-html/r700.html}
    META files}. *)
module Meta : sig

  (** {1 META files} *)

  type t
  (** The type for Findlib META files. *)

  val to_string : t -> string
  (** [to_string meta] is [m] as a string. *)

  val of_project : Assemblage.project -> t
  (** [of_project p] is a META file for project [p]. *)
end

(** Merlin support.

    Generate {{:https://github.com/the-lambda-church/merlin}Merlin}
    {{:https://github.com/the-lambda-church/merlin#merlin-project}
    project files}. *)
module Merlin : sig

  (** {1 Merlin project file} *)

  type directive =
    [ `REC | `S of string | `B of string | `PKG of string
    | `FLG of string list | `EXT of string list ]
  (** The type for Merlin directives. *)

  type t = [ `Comment of string | `Blank | directive ] list
  (** The type for Merlin project files. *)

  val to_string : t -> string
  (** [to_string m] is [m] as a string. *)

  val of_project : project -> t
  (** [of_project p] is a merlin file for project [p]. *)
end
