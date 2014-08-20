(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 * Copyright (c) 2014 Louis Gesbert
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

let verbose =
  try Sys.getenv "VERBOSE" <> ""
  with Not_found -> false

let color_tri_state =
  try match Sys.getenv "COLOR" with
    | "always" -> `Always
    | "never"  -> `Never
    | _        -> `Auto
  with
  | Not_found  -> `Auto

let with_color =
  ref (color_tri_state <> `Never)

type text_style =
  [ `Bold
  | `Underline
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White ]

let color (c: text_style) s =
  if not !with_color then s else
    let code = match c with
      | `Bold      -> "01"
      | `Underline -> "04"
      | `Black     -> "30"
      | `Red       -> "31"
      | `Green     -> "32"
      | `Yellow    -> "33"
      | `Blue      -> "1;34"
      | `Magenta   -> "35"
      | `Cyan      -> "36"
      | `White     -> "37"
    in
    Printf.sprintf "\027[%sm%s\027[m" code s

let show fmt =
  ksprintf (fun str ->
      printf "%s %s\n%!" (color `Cyan "+") str
    ) fmt

let fatal_error i fmt =
  ksprintf (fun str ->
     eprintf "%s: %s\n%!" (color `Red "ERROR") str;
     exit i
    ) fmt

let has_cmd cmd =
  Sys.command (Printf.sprintf "type %s 1>/dev/null 2>/dev/null" cmd) = 0

let read file =
  try
    let ic = open_in file in
    let lines =
      let lines = ref [] in
      try while true do
          lines := input_line ic :: !lines
        done;
        assert false
      with End_of_file ->
        List.rev !lines in
    close_in ic;
    lines
  with Sys_error e ->
    fatal_error 1 "while reading %s: %s" file e

let temp () =
  try
    let file = Filename.temp_file (Filename.basename Sys.argv.(0)) ".out" in
    at_exit (fun () -> Sys.remove file);
    file
  with Sys_error e ->
    fatal_error 1 "while creating temp file: %s" e

let exec ?(verbose=verbose) fmt =
  ksprintf (fun cmd ->
      if verbose then printf "%s %s\n" (color `Yellow "=>") cmd;
      let i = Sys.command cmd in
      if i <> 0 then fatal_error i "`%s' exited with code %d" cmd i
    ) fmt

let try_exec fmt =
  ksprintf (fun cmd ->
      let i = Sys.command (sprintf "%s 1>/dev/null 2>/dev/null" cmd) in
      i = 0
    ) fmt

let exec_output ?verbose fmt =
  ksprintf (fun cmd ->
      let file = temp () in
      exec ?verbose "%s > %s" cmd file;
      read file
    ) fmt

let in_dir dir fn =
  let pwd = Sys.getcwd () in
  try
    if Sys.file_exists dir && Sys.is_directory dir then (
      Sys.chdir dir;
      let r = fn () in
      Sys.chdir pwd;
      r
    ) else
      failwith (sprintf "%s does not exist" dir)
  with e ->
    Sys.chdir pwd;
    raise e
