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

type level = Show | Error | Warning | Info | Debug

let level = ref (Some Warning)
let set_level l = level := l
let level () = !level

let should_log l = match level () with
| None -> false | Some l' -> l <= l'

let show_ppf = ref Format.std_formatter
let err_ppf = ref Format.err_formatter
let warn_ppf = ref Format.err_formatter
let info_ppf = ref Format.err_formatter
let debug_ppf = ref Format.err_formatter

let set_formatter spec ppf = match spec with
| `Level Show -> show_ppf := ppf
| `Level Error -> err_ppf := ppf
| `Level Warning -> warn_ppf := ppf
| `Level Info -> info_ppf := ppf
| `Level Debug -> debug_ppf := ppf
| `All ->
    show_ppf := ppf; err_ppf := ppf; warn_ppf := ppf; info_ppf := ppf;
    debug_ppf := ppf

(* Logging *)

let dumb = Format.err_formatter (* any will do *)
let err_count = ref 0
let warn_count = ref 0

let kmsg ?header k l fmt =
  let default d = match header with None -> d | Some h -> h in
  let k _ = k () in
  if not (should_log l) then Format.ikfprintf k dumb fmt else
  let pp_msg ppf style label fmt =
    Format.kfprintf k ppf
      ("[%a] @[" ^^ fmt ^^ "@]@.") (As_fmt.pp_styled_str style) label
  in
  match l with
  | Show ->
      begin match header with
      | None -> Format.kfprintf k !show_ppf ("@[" ^^ fmt ^^ "@]@.")
      | Some h -> pp_msg !show_ppf `Bold h fmt
      end
  | Error ->
      incr err_count; pp_msg !err_ppf `Red (default "ERROR") fmt
  | Warning ->
      incr warn_count; pp_msg !warn_ppf `Yellow (default "WARNING") fmt
  | Info ->
      pp_msg !info_ppf `Blue (default "INFO") fmt
  | Debug ->
      pp_msg !debug_ppf `Green (default "DEBUG") fmt

let msg ?header l fmt = kmsg ?header (fun () -> ()) l fmt
let msg_driver_fault ?header l fmt =
  msg ?header l ("[%a] " ^^ fmt) (As_fmt.pp_styled_str `Red) "DRIVER FAULT"

let show ?header fmt = msg ?header Show fmt
let err ?header fmt = msg ?header Error fmt
let warn ?header fmt = msg ?header Warning fmt
let info ?header fmt = msg ?header Info fmt
let debug ?header fmt = msg ?header Debug fmt

(* Log monitoring *)

let err_count () = !err_count
let warn_count () = !warn_count
