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

(** Part for compilation units.

    See {!Assemblage.Part.Unit}. *)

type ocaml_interface = [ `Normal | `Opaque | `Hidden ]
type ocaml_unit = [ `Ml | `Mli | `Both ]
type c_unit = [ `C | `H | `Both ]

type kind = [ `OCaml of ocaml_unit * ocaml_interface | `C of c_unit | `Js ]

val kind : [< `Unit] As_part.t -> kind
val src_dir : [< `Unit] As_part.t -> As_path.rel

val create :
  ?cond:bool As_conf.value -> ?args:As_args.t -> ?deps:'a As_part.t list ->
  ?src_dir:(As_path.rel) -> string -> kind -> [> `Unit] As_part.t

val of_base : src_dir:(As_path.rel) -> kind -> [`Base] As_part.t ->
  [> `Unit] As_part.t

(*
val ocaml : 'a As_part.t -> [> `Unit] As_part.t option
val c : 'a As_part.t -> [> `Unit] As_part.t option
val js : 'a As_part.t -> [> `Unit] As_part.t option
*)
