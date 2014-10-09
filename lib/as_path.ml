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

let str = Printf.sprintf
let err_not_relative = "not a relative path"
let err_not_absolute = "not an absolute path"
let err_no_ext seg = str "no file extension in last segment (%s)" seg

(* File paths *)

type filename = string
type segs = string list
type rel = [ `Rel of segs ]
type abs = [ `Abs of segs ]
type t = [ abs | rel ]

let segs = function `Abs segs | `Rel segs -> segs
let map f = function
| `Rel segs -> `Rel (f segs)
| `Abs segs -> `Abs (f segs)

let current = `Rel []
let root = `Abs []
let is_current = function `Rel [] -> true | _ -> false
let is_root = function `Abs [] -> true | _ -> false
let is_rel = function `Rel _ -> true | _ -> false
let is_abs = function `Abs _ -> true | _ -> false
let as_rel = function `Rel _ as v -> v | _ -> invalid_arg err_not_relative
let as_abs = function `Abs _ as v -> v | _ -> invalid_arg err_not_absolute
let as_path p = (p :> t)

let basename p = match List.rev (segs p) with [] -> "" | seg :: _ -> seg
let dirname p =
  let dirname segs = match List.rev segs with
  | [] -> [] | seg :: rsegs -> List.rev rsegs
  in
  map dirname p

let concat_seg p = function
| "" -> (p :> t)
| seg ->
    let cat segs = List.rev (seg :: List.rev segs) in
    map cat p

let concat p (`Rel segs') =
  let cat segs = List.(rev_append (rev segs) segs') in
  (map cat p :> t)

let ( / ) = concat_seg
let ( // ) = concat

let file f = as_rel (concat_seg current f)
let dir d = as_rel (concat_seg current d)

let rel_of_segs ss = `Rel (segs (List.fold_left concat_seg current ss))
let abs_of_segs ss = `Abs (segs (List.fold_left concat_seg root ss))

let to_abs ?(rel_base = root) p = match p with
| `Rel ss -> `Abs List.(rev_append (rev (segs rel_base)) ss)
| `Abs _ as p -> p

let to_string = function
| `Rel segs -> String.concat Filename.dir_sep segs
| `Abs segs -> "/" ^ String.concat Filename.dir_sep segs

let of_string s =                                (* N.B. collapses // to / *)
  let rec split sep acc j =
    let i = try String.rindex_from s j sep with Not_found -> -1 in
    if (i = -1) then (String.sub s 0 (j + 1)) :: acc else
    let p = String.sub s (i + 1) (j - i) in
    let acc' = if p <> "" then p :: acc else acc in
    split sep acc' (i - 1)
  in
  match split Filename.dir_sep.[0] [] (String.length s - 1) with
  | "" :: segs -> `Abs segs
  | segs -> `Rel segs

(* File system queries *)

let exists p = try Sys.file_exists (to_string p) with Sys_error _ -> false
let is_dir p = try Sys.is_directory (to_string p) with Sys_error _ -> false
let is_file p =
  try not (Sys.is_directory (to_string p)) with Sys_error _ -> false

(* File extensions *)

type ext =
  [ `Ml_dep | `Mli_dep | `Ml | `Mli | `C | `H | `Js | `Cmi | `Cmo | `Cmx | `O
  | `Cmt | `Cmti | `Cma | `Cmxa | `Cmxs | `A | `So | `Byte | `Native
  | `Ext of string ]

let ext_to_string = function
| `Ml_dep -> "mldep" | `Mli_dep -> "mlidep" | `Ml -> "ml" | `Mli -> "mli"
| `C -> "c" | `H -> "h" | `Js -> "js" | `Cmi -> "cmi" | `Cmo -> "cmo"
| `Cmx -> "cmx" | `O -> "o" | `Cmt -> "cmt" | `Cmti -> "cmti" | `Cma -> "cma"
| `Cmxa -> "cmxa" | `Cmxs -> "cmxs" | `A -> "a" | `So -> "so"
| `Byte -> "byte" | `Native -> "native" | `Ext ext -> ext

let ext_of_string = function
| "mldep" -> `Ml_dep  | "mlidep" -> `Mli_dep | "ml" -> `Ml | "mli" -> `Mli
| "c" -> `C | "h" -> `H | "js" -> `Js | "cmi" -> `Cmi | "cmo" -> `Cmo
| "cmx" -> `Cmx | "o" -> `O | "cmt" -> `Cmt | "cmti" -> `Cmti | "cma" -> `Cma
| "cmxa" -> `Cmxa | "cmxs" -> `Cmxs | "a" -> `A | "so" -> `So
| "byte" -> `Byte | "native" -> `Native | ext -> `Ext ext

let ext p = match List.rev (segs p) with
| [] -> None
| seg :: _ ->
    try
      let i = String.rindex seg '.' in
      let ext = String.sub seg (i + 1) (String.length seg - i - 1) in
      Some (ext_of_string ext)
    with Not_found -> None

let get_ext p = match ext p with
| Some ext -> ext
| None ->
    let seg = match List.rev (segs p) with [] -> "" | seg :: _ -> seg in
    invalid_arg (err_no_ext seg)

let has_ext e p = match ext p with None -> false | Some e' -> e = e'

let chop_ext p =
  let chop segs = match List.rev segs with
  | [] -> []
  | seg :: segs' ->
      try
        let i = String.rindex seg '.' in
        let name = String.sub seg 0 i in
        List.rev (name :: segs')
      with Not_found -> segs
  in
  map chop p

let add_ext p e =
  let suff = "." ^ ext_to_string e in
  let add_ext segs = match List.rev segs with
  | [] -> [suff]
  | seg :: rsegs -> List.rev ((seg ^ suff) :: rsegs)
  in
  map add_ext p

let ( + ) = add_ext
