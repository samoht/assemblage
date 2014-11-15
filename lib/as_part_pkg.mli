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

(** Package part.

    See {!Assemblage.Pkg} *)

(** {1 Metadata} *)

type other = [ `Other of string * As_args.t ]
type kind = [ `OCaml of [`OCamlfind | other ]
              | `C of [ `Pkg_config | other ]]

val pp_kind : Format.formatter -> kind -> unit
val kind : [< `Pkg] As_part.t -> kind
val lookup : [< `Pkg] As_part.t -> As_args.t
val ocaml : 'a As_part.t -> [> `Pkg] As_part.t option
val c : 'a As_part.t -> [> `Pkg] As_part.t option

(** {1 Packages} *)

val v :
  ?usage:As_part.usage -> ?exists:bool As_conf.value -> ?args:As_args.t ->
  string -> kind -> [> `Pkg] As_part.t
