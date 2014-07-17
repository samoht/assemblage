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

type args = string list
type atom =
  { available : As_features.t;
    pp_byte : args;
    pp_native : args;
    comp_byte : args;
    comp_native : args;
    comp_js : args;
    link_byte : args;
    link_native : args;
    link_js : args;
    link_shared : args;
    c : args; }

type t = atom list
let (@@@) fs fs' = fs @ fs'

let create ?(available = As_features.true_)
    ?(pp_byte = []) ?(pp_native = [])
    ?(comp_byte = []) ?(comp_native = []) ?(comp_js = [])
    ?(link_byte = []) ?(link_native = []) ?(link_js = []) ?(link_shared = [])
    ?(c = []) ()
  =
  [ { available;
      pp_byte; pp_native;
      comp_byte; comp_native; comp_js;
      link_byte; link_native; link_js; link_shared;
      c; } ]


let empty = create ()

let get _ field fs =
  let rec loop acc = function
  | [] -> List.flatten (List.rev acc)
  | atom :: atoms ->
      let available = true (* FIXME As_features.eval ctx t.features *) in
      let acc' = if available then ((field atom) :: acc) else acc in
      loop acc' atoms
  in
  loop [] fs

(* FIXME these functions need an evaluation context as argument *)

let ctx = ()
let pp_byte fs = get ctx (fun a -> a.pp_byte) fs
let pp_native fs = get ctx (fun a -> a.pp_native) fs
let comp_byte fs = get ctx (fun a -> a.comp_byte) fs
let comp_native fs = get ctx (fun a -> a.comp_native) fs
let comp_js fs = get ctx (fun a -> a.comp_js) fs
let link_byte fs = get ctx (fun a -> a.link_byte) fs
let link_js fs = get ctx (fun a -> a.comp_js) fs
let link_native fs = get ctx (fun a -> a.link_native) fs
let link_shared fs = get ctx (fun a -> a.link_shared) fs
let c fs = get ctx (fun a -> a.c) fs

(* Built-in flags *)

let debug =
  let f = ["-g"] in
  let js_f = [ "-pretty"; "-debuginfo"; "-sourcemap"] in
  create
    ~comp_byte:f ~comp_native:f ~comp_js:js_f
    ~link_byte:f ~link_native:f ~link_shared:f
    ~c:f ()

let annot =
  let f = ["-bin-annot"] in
  create ~comp_byte:f ~comp_native:f ()

let linkall =
  let f = ["-linkall"] in
  create ~link_byte:f ~link_native:f ~link_js:f ()

let warn_error =
  let f = ["-warn-error A-44-4 -w A-44-4"] in
  create ~comp_byte:f ~comp_native:f ()

let thread =
  let f = ["-thread"] in
  create
    ~comp_byte:f ~comp_native:f
    ~link_byte:f ~link_native:f ~link_shared:f ()

let cclib args =
  let f = List.map (sprintf "-cclib %s") args in
  create
    ~link_byte:f ~link_native:f ~link_shared:f
    ~c:args ()

let ccopt args =
  let f = List.map (sprintf "-ccopt %s") args in
  create
    ~comp_byte:f ~comp_native:f
    ~link_byte:f ~link_native:f ~link_shared:f
    ~c:args ()

let stub s = create
    ~link_byte:[sprintf "-cclib -l%s -dllib -l%s" s s]
    ~link_native:[sprintf "-cclib -l%s" s]
    ~link_shared:[] ()
