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

(* Log level and output *)

type level = Show | Marker | Error | Warning | Info | Debug

let level = ref (Some Info)
let set_level l = level := l
let level () = !level

let should_log l = match level () with
| None -> false | Some l' -> l <= l'

let show_ppf = ref Format.std_formatter
let marker_ppf = ref Format.err_formatter
let err_ppf = ref Format.err_formatter
let warn_ppf = ref Format.err_formatter
let info_ppf = ref Format.err_formatter
let debug_ppf = ref Format.err_formatter

let set_formatter spec ppf = match spec with
| `Level Show -> show_ppf := ppf
| `Level Marker -> marker_ppf := ppf
| `Level Error -> err_ppf := ppf
| `Level Warning -> warn_ppf := ppf
| `Level Info -> info_ppf := ppf
| `Level Debug -> debug_ppf := ppf
| `All ->
    show_ppf := ppf; marker_ppf := ppf; err_ppf := ppf; warn_ppf := ppf;
    info_ppf := ppf; debug_ppf := ppf

(* Logging *)

let dumb = Format.err_formatter (* any will do *)
let err_count = ref 0
let warn_count = ref 0

let kmsg k l fmt =
  let k _ = k () in
  if not (should_log l) then Format.ikfprintf k dumb fmt else
  let pp_msg ppf style label fmt =
    Format.kfprintf k ppf
      ("%a: @[" ^^ fmt ^^ "@]@.") (As_fmt.(pp_styled style pp_str)) label
  in
  match l with
  | Show -> Format.kfprintf k !show_ppf ("@[" ^^ fmt ^^ "@]@.")
  | Marker -> Format.kfprintf k !marker_ppf ("@[" ^^ fmt ^^ "@]@.")
  | Error -> incr err_count; pp_msg !err_ppf `Red "ERROR" fmt
  | Warning -> incr warn_count; pp_msg !warn_ppf `Red "WARNING" fmt
  | Info -> pp_msg !info_ppf `Blue "INFO" fmt
  | Debug -> pp_msg !debug_ppf `Green "DEBUG" fmt

let msg l fmt = kmsg (fun () -> ()) l fmt
let show fmt = msg Show fmt
let mark fmt = msg Marker fmt
let err fmt = msg Marker fmt
let warn fmt = msg Marker fmt
let info fmt = msg Marker fmt
let debug fmt = msg Marker fmt

(* Log monitoring *)

let err_count () = !err_count
let warn_count () = !warn_count
