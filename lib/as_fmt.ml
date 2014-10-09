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

(* Formatters *)

type 'a formatter = Format.formatter -> 'a -> unit

let pp ppf fmt = Format.fprintf ppf fmt
let rpp fmt ppf = Format.fprintf ppf fmt
let nop fmt ppf = ()
let pp_cut = Format.pp_print_cut
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_bool = Format.pp_print_bool
let pp_int = Format.pp_print_int
let pp_larrow ppf () = pp_str ppf "<=="
let pp_rarrow ppf () = pp_str ppf "==>"
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ppf = function
| v :: vs -> pp_v ppf v; pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs
| [] -> ()

let pp_white_str ~spaces ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pp_text = pp_white_str ~spaces:true
let pp_lines = pp_white_str ~spaces:false

(* Styled formatting *)

type style_tags = [ `Ansi | `None ]
type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White ]


let style_tags = ref `None
let set_style_tags s = style_tags := s
let style_tags () = !style_tags

let ansi_style_code = function
| `Bold -> "01"
| `Underline -> "04"
| `Black -> "30"
| `Red -> "31"
| `Green -> "32"
| `Yellow -> "33"
| `Blue -> "1;34"
| `Magenta -> "35"
| `Cyan -> "36"
| `White -> "37"

let pp_styled style pp_v = match style_tags () with
| `None -> pp_v
| `Ansi -> fun ppf -> pp ppf "\027[%sm%a\027[m" (ansi_style_code style) pp_v

let pp_styled_str style = pp_styled style pp_str
