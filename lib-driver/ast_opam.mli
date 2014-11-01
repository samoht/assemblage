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

(** OPAM support.

    See {!Assemblage_tools.Opam}. *)

(** {1 Metadata synchronization} *)

module Sync : sig

end

(** {1 Install file} *)

module Install : sig

  (** {1 Install file} *)

  type move
  val move : ?maybe:bool -> ?dst:string -> string -> move

  type field_elt =
    [ `Bin of move | `Doc of move | `Etc of move | `Lib of move | `Man of move
    | `Misc of move | `Sbin of move | `Share of move | `Share_root of move
    | `Stublibs of move | `Toplevel of move ]

  type t = [`Header of string option ] * field_elt list

  val to_string : t -> string
  val of_project : ?add:field_elt list -> Assemblage.project -> t
end
