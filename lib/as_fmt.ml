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
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)
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

(* Conditional UTF-8 formatting

   Note that it would be interesting to investigate if the Format tags
   mechanism can be used to pretty-print multi-byte UTF-8 correctly --
   that for a loose definition of correct, you'd still need the
   unicode segmentation algorithm for visual correctness. *)

let utf8_enabled, set_utf8_enabled =
  let enabled = ref false in
  (fun () -> !enabled), (fun b -> enabled := b)

let pp_if_utf8 pp_u pp ppf v = (if utf8_enabled () then pp_u else pp) ppf v

(* Styled formatting *)

type style_tags = [ `Ansi | `None ]
type style =
  [ `Bold | `Underline | `Black | `Red | `Green | `Yellow | `Blue | `Magenta
  | `Cyan | `White | `None ]

let style_tags, set_style_tags =
  let style_tags = ref `None in
  (fun () -> !style_tags), (fun s -> style_tags := s)

let ansi_style_code = function
| `Bold -> "\027[01m"
| `Underline -> "\027[04m"
| `Black -> "\027[30m"
| `Red -> "\027[31m"
| `Green -> "\027[32m"
| `Yellow -> "\027[33m"
| `Blue -> "\027[1;34m"
| `Magenta -> "\027[35m"
| `Cyan -> "\027[36m"
| `White -> "\027[37m"
| `None -> "\027[m"

let style_tag_funs =
  { Format.mark_open_tag = (fun t -> t);
    Format.mark_close_tag = (fun _ -> "\027[m");
    Format.print_open_tag = (fun _ -> ());
    Format.print_close_tag = (fun _ -> ()); }

let pp_styled style pp_v ppf = match style_tags () with
| `None -> pp_v ppf
| `Ansi ->
    begin fun ppf ->
      Format.pp_set_formatter_tag_functions ppf style_tag_funs;
      Format.pp_set_mark_tags ppf true;
      Format.kfprintf
        (fun ppf -> Format.pp_close_tag ppf ()) ppf
        "%a%a" Format.pp_open_tag (ansi_style_code style) pp_v
    end ppf

let pp_styled_str style = pp_styled style pp_str
