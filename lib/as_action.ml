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

type action = {
  dir: string option;
  cmd: string;
}

type kind = [ `Ml | `Mli | `Cmo | `Cmi | `Cmx | `O | `C | `Js | `Other of string]

let string_of_kind = function
| `Ml -> "ml"
| `Mli -> "mli"
| `Cmo -> "cmo"
| `Cmi -> "cmi"
| `Cmx -> "cmx"
| `O -> "o"
| `C -> "c"
| `Js -> "js"
| `Other s -> s

type t = As_resolver.t -> (kind list * action) list

let empty _ = []

let create ?dir fmt =
  ksprintf (fun cmd ->
      { dir; cmd }
    ) fmt

let actions (t:t) r =
  List.map (fun (kinds, action) ->
      match action.dir with
      | None   -> kinds, [action.cmd]
      | Some d -> kinds, [
          sprintf "mkdir -p %s" d;
          sprintf "cd %s && %s" d action.cmd
        ]
    ) (t r)
