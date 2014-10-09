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

open Printf

let (|>) x f = f x

(* Project *)

type t =
  { name : string;
    cond : As_cond.t;
    args : As_args.t;
    parts : As_part.kind As_part.t list; }

let name t = t.name
let parts t = t.parts

let create ?(cond = As_cond.true_) ?(args = As_args.empty)
    name parts =
  { name; cond; args;
    parts = (parts :> As_part.kind As_part.t list); }

let unionmap fn t =
  List.fold_left (fun set t ->
      As_cond.Set.union set (fn t)
    ) As_cond.Set.empty t

let cond_atoms t =
  let all =
    unionmap (fun x -> As_cond.atoms
                 (As_part.cond x)) t.parts
  in
  As_cond.Set.union As_cond.builtin all
