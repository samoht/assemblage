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

let to_string = function
| `Rel segs -> String.concat Filename.dir_sep segs
(* FIXME windows what's the root ? *)
| `Abs segs -> (Filename.dir_sep ^ String.concat Filename.dir_sep segs)

let err_not_rel abs = str "not a relative path %s" (to_string abs)
let err_not_abs rel = str "not an absolute path %s" (to_string rel)
let err_no_ext seg = str "no file extension in last segment (%s)" seg

(* File paths *)

type filename = string
type segs = string list
type rel = [ `Rel of segs ]
type abs = [ `Abs of segs ]

module Path = struct
  type t = [ abs | rel ]
  let compare = compare
end

type t = Path.t

let segs = function `Abs segs | `Rel segs -> segs
let map f = function
| `Rel segs -> `Rel (f segs)
| `Abs segs -> `Abs (f segs)

let current = `Rel []
let root = `Abs []
let dash = `Rel ["-"]
let is_current = function `Rel [] -> true | _ -> false
let is_root = function `Abs [] -> true | _ -> false
let is_rel = function `Rel _ -> true | _ -> false
let is_abs = function `Abs _ -> true | _ -> false
let is_dash = function `Rel [ "-" ] -> true | _ -> false
let as_rel = function `Rel segs as v -> v | v -> invalid_arg (err_not_rel v)
let as_abs = function `Abs segs as v -> v | v -> invalid_arg (err_not_abs v)
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

let equal p p' = p = p'
let compare = Path.compare

(* FIXME `{to,of}_string,quote` are we doing the right things ?  *)

let of_string s =                                (* N.B. collapses // to / *)
  (* FIXME unquote ? *)
  match As_string.split ~sep:Filename.dir_sep s with
  | "" :: segs -> abs_of_segs segs   (* FIXME windows ?? *)
  | segs -> rel_of_segs segs

let quote p = Filename.quote (to_string p)

let pp ppf p = As_fmt.pp_str ppf (to_string p)

(* File extensions *)

type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]

let ext_to_string = function
| `A -> "a" | `Byte -> "byte" | `C -> "c" | `Cma -> "cma" | `Cmi -> "cmi"
| `Cmo -> "cmo" | `Cmt -> "cmt" | `Cmti -> "cmti" | `Cmx -> "cmx"
| `Cmxa -> "cmxa" | `Cmxs -> "cmxs" | `Css -> "css" | `Dll -> "dll"
| `Exe -> "exe" | `Gif -> "gif" | `H -> "h" | `Html -> "html"
| `Install -> "install" | `Img -> "img" | `Jpeg -> "jpeg" | `Js -> "js"
| `Json -> "json" | `Lib -> "lib" | `Md -> "md" | `Ml -> "ml"
| `Ml_dep -> "ml-dep" | `Ml_pp -> "ml-pp" | `Mli -> "mli"
| `Mli_dep -> "mli-dep" | `Mli_pp -> "mli-pp" | `Native -> "native"
| `O -> "o" | `Opt -> "opt" | `Png -> "png" | `Sh -> "sh" | `So -> "so"
| `Tar -> "tar" | `Tbz -> "tbz" | `Xml -> "xml" | `Zip -> "zip"
| `Ext ext -> ext

let ext_of_string = function
| "a" -> `A | "byte" -> `Byte | "c" -> `C | "cma" -> `Cma | "cmi" -> `Cmi
| "cmo" -> `Cmo | "cmt" -> `Cmt | "cmti" -> `Cmti | "cmx" -> `Cmx
| "cmxa" -> `Cmxa | "cmxs" -> `Cmxs | "css" -> `Css | "dll" -> `Dll
| "exe" -> `Exe | "gif" -> `Gif | "h" -> `H | "html" -> `Html
| "install" -> `Install | "img" -> `Img | "jpeg" -> `Jpeg | "js" -> `Js
| "json" -> `Json | "lib" -> `Lib | "md" -> `Md | "ml" -> `Ml
| "ml-dep" -> `Ml_dep | "ml-pp" -> `Ml_pp | "mli" -> `Mli
| "mli-dep" -> `Mli_dep | "mli-pp" -> `Mli_pp | "native" -> `Native
| "o" -> `O | "opt" -> `Opt | "png" -> `Png | "sh" -> `Sh | "so" -> `So
| "tar" -> `Tar | "tbz"  -> `Tbz | "xml" -> `Xml | "zip" -> `Zip
| ext -> `Ext ext

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

let add_ext p e =
  let suff = ext_to_string e in
  let add_ext segs = match List.rev segs with
  | [] -> [str ".%s" suff]
  | seg :: rsegs -> List.rev (str "%s.%s" seg suff :: rsegs)
  in
  map add_ext p

let rem_ext p =
  let rem segs = match List.rev segs with
  | [] -> []
  | seg :: segs' ->
      try
        let i = String.rindex seg '.' in
        let name = String.sub seg 0 i in
        List.rev (name :: segs')
      with Not_found -> segs
  in
  map rem p

let change_ext p e = add_ext (rem_ext p) e
let ( + ) = add_ext
let ( -+ ) = change_ext

(* Path sets and maps *)

module Set = struct
  include Set.Make (Path)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = Map.Make (Path)
