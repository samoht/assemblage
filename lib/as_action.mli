(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Actions to generate new files. *)

type action

val create: ?dir:string -> ('a, unit, string, action) format4 -> 'a
val seq: action list -> action
val mkdir: string -> action
val link: source:string -> target:string -> action

type 'a t = 'a -> As_resolver.t -> As_flags.t -> action

type file =
  [ `Dep of [`Ml|`Mli]
  | `Ml | `Mli | `C | `Js
  | `Cmt | `Cmti
  | `Cmi | `Cmo | `Cmx | `O
  | `So | `Cma | `Cmxa | `Cmxs
  | `A | `Byte | `Native
  | `Dir
  | `Source of file
  | `Ext of string
  | `Other of (string -> string) ]

module FileSet: sig
  include Set.S with type elt = file
  val to_list: t -> file list
  val of_list: file list -> t
end

type 'a node =
  [ `Self of file
  | `Phony of string
  | `N of 'a * file ]

type 'a rule = {
  phase  : As_flags.phase;
  targets: 'a node list;
  prereqs: 'a node list;
  action : 'a t;
}

val rule:
  phase:As_flags.phase ->
  targets:'a node list ->
  prereqs:'a node list ->
  'a t -> 'a rule

val string_of_file: string -> file -> string

val empty: 'a t

val run: 'a t -> 'a -> As_resolver.t -> As_flags.t -> string list
