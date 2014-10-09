(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Daniel C. Buenzli
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

(** Setup environment.

    The setup environment defines the default setup configuration. *)

type t
(** The type for build environments. *)

val create :
  ?atomic_conds:(Assemblage.Cond.atom * bool) list ->
  ?args:Assemblage.args ->
  ?build_dir: string ->
  unit -> t
(** {ul
    {- [build_dir] is the location of the generated files. [None]
      means the files stays in the same directory.}} *)

val args : t -> Assemblage.args
(** Return the global comand-line args. *)

val build_dir : t -> string
(** Return the directory where build artifacts are generated. *)

val atomic_conds : t -> (Assemblage.Cond.atom * bool) list
(** Return a list of feature with the values they are set to. *)

val term : Assemblage.Cond.Set.t -> t Cmdliner.Term.t
(** [term as] is a command line for atomic conditions [as] resulting
    in an environement value. *)
