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
    version : string;
    available : As_features.t;
    flags : As_flags.t;
    components : As_component.t list; }

let name t = t.name
let version t = t.version
let components t = t.components

let create ?(available = As_features.true_) ?(flags = As_flags.empty)
    ?version name components =
  let version = match version with
  | Some v -> v
  | None   ->
      match As_git.describe () with
      | Some v -> v
      | None   ->
          match As_git.head () with
          | Some v -> v
          | None   -> "version-not-set"
  in
  let components = As_component.closure ~link:false components in
  { name; version; available; flags; components; }

let unionmap fn t =
  List.fold_left (fun set t ->
      As_features.(set ++ (fn t))
    ) As_features.Set.empty t

let features t =
  let all =
    unionmap (fun x -> As_features.atoms
                 (As_component.available x)) t.components
  in
  As_features.(builtin ++ all)
