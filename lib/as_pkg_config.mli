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

(** pkg-config invocations. *)

type syntax = [ `Shell | `Makefile ]
(** The type for invocation syntax. [`Shell] returns a [sh] command
    to run while [`Makefile] wraps it in $(shell ...). *)

type mode = [ `Static | `Dynamic of syntax ]
(** The type for invocation modes.
    {ul
    {- [`Static], calls [pkg-config] directly and returns the result.}
    {- [`Dynamic], returns the [pkg-config] invocation to run to get
       the results.}} *)

val pkgs_args : mode:mode -> string list -> Assemblage.Args.t
(** [pkgs_args mode pkgs] returns the args for packages [pkgs]. *)
