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

type s = string list

type t = {
  comp_byte  : s;
  comp_native: s;
  pp_byte    : s;
  pp_native  : s;
  link_byte  : s;
  link_native: s;
  c          : s;
}

let (@) f g = {
  comp_byte   = f.comp_byte   @ g.comp_byte  ;
  comp_native = f.comp_native @ g.comp_native;
  pp_byte     = f.pp_byte     @ g.pp_byte    ;
  pp_native   = f.pp_native   @ g.pp_native  ;
  link_byte   = f.link_byte   @ g.link_byte  ;
  link_native = f.link_native @ g.link_native;
  c           = f.c           @ g.c          ;
}

let create
    ?(comp_byte=[]) ?(comp_native=[])
    ?(pp_byte=[])   ?(pp_native=[])
    ?(link_byte=[]) ?(link_native=[])
    ?(c=[])
    () =
  { comp_byte; comp_native;
    pp_byte; pp_native;
    link_byte; link_native;
    c; }

let empty =
  { comp_byte = []; comp_native = [];
    pp_byte = []; pp_native = [];
    link_byte = []; link_native = [];
    c = ["-fPIC -Wall -O3"]; }

let comp_byte t = t.comp_byte

let comp_native t = t.comp_native

let pp_byte t = t.pp_byte

let pp_native t = t.pp_native

let link_byte t = t.link_byte

let link_native t = t.link_native

let c t = t.c

let debug =
  let f = ["-g"] in
  { empty with
    comp_byte   = f; comp_native = f;
    link_byte   = f; link_native = f;
    c = ["-fPIC -Wall -g"];
  }

let annot =
  let f = ["-bin-annot"] in
  { empty with comp_byte = f; comp_native = f }

let linkall =
  let f = ["-linkall"] in
  { empty with link_byte = f; link_native = f }

let warn_error =
  let f = ["-warn-error A-44-4 -w A-44-4"] in
  { empty with comp_byte = f; comp_native = f }

let thread =
  let f = ["-thread"] in
  { empty with
    comp_byte = f; comp_native = f;
    link_byte = f; link_native = f; }

let cclib args =
  let f = List.map (sprintf "-cclib %s") args in
  { empty with
    link_byte = f; link_native = f; c = args;
  }

let ccopt args =
  let f = List.map (sprintf "-ccopt %s") args in
  { empty with
    comp_byte = f; comp_native = f;
    link_byte = f; link_native = f;
    c = args;
  }

let stub s =
  { empty with
    link_byte   = [sprintf "-cclib -l%s -dllib -l%s" s s];
    link_native = [sprintf "-cclib -l%s" s]
  }
