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

type one_action = {
  dir: string option;
  cmd: string;
}

type action = one_action list

type file =
  [ `Dep of [`Ml|`Mli]
  | `Ml | `Mli | `C | `Js
  | `Cmi | `Cmo | `Cmx | `O
  | `Cmt | `Cmti
  | `So | `Cma | `Cmxa | `Cmxs
  | `A | `Byte | `Native
  | `Dir
  | `Source of file
  | `Ext of string
  | `Other of (string -> string) ]

let rec string_of_file name (f:file) = match f with
| `Dep `Ml -> name ^ ".ml.d"
| `Dep `Mli -> name ^ ".mli.d"
| `Ml -> name ^ ".ml"
| `Mli -> name ^ ".mli"
| `C -> name ^ ".c"
| `Js -> name ^ ".js"
| `Cmi -> name ^ ".cmi"
| `Cmo -> name ^ ".cmo"
| `Cmx -> name ^ ".cmx"
| `Cmt -> name ^ ".cmt"
| `Cmti -> name ^ ".cmti"
| `O -> name ^ ".o"
| `So -> "dll" ^ name ^ ".so"
| `Cma -> name ^ ".cma"
| `Cmxa -> name ^ ".cmxa"
| `Cmxs -> name ^ ".cmxs"
| `A -> name ^ ".a"
| `Byte -> name ^ ".byte"
| `Native -> name ^ ".native"
| `Source f -> string_of_file name f
| `Ext e -> name ^ "." ^ e
| `Other f -> f name
| `Dir -> ""

module FileSet = struct
  include Set.Make(struct
      type t = file
      let compare x y =
        String.compare (string_of_file "xx" x) (string_of_file "xx" y)
    end)
  let to_list = elements
  let of_list ss = List.fold_left (fun acc s -> add s acc) empty ss
end


type 'a t = 'a -> As_resolver.t -> As_flags.t -> action

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

let rule ~phase ~targets ~prereqs action =
  { phase; targets; prereqs; action }

let empty _ _ _ = []

let create ?dir fmt =
  ksprintf (fun cmd ->
      [ { dir; cmd } ]
    ) fmt

let seq actions = List.concat actions

let mkdir r dir =
  let cmd = As_resolver.mkdir r in
  create "%s %s" cmd dir

let link r ~source ~target =
  let cmd = As_resolver.ln r in
  create "%s %s %s" cmd source target

let run (t:'a t) x r f =
  List.map (fun action -> match action.dir with
    | None   -> action.cmd
    | Some d -> sprintf "cd %s && %s" d action.cmd
    ) (t x r f)
