1(*
 * Copyright (c) 2014 Daniel C. Bünzli.
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


let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let red_s = red "%s"
let green_s = green "%s"
let yellow_s = yellow "%s"
let blue_s = blue "%s"

let fatal_error i fmt =
  ksprintf (fun str ->
     eprintf "%s: %s\n" (red_s "ERROR") str;
     exit i
    ) fmt

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

let write file s =
  try
    let oc = open_out file in
    output_string oc s;
    close_out oc
  with Sys_error e ->
    fatal_error 1 "while writing %s: %s" file e

let temp () =
  try
    let file = Filename.temp_file (Filename.basename Sys.argv.(0)) ".out" in
    at_exit (fun () -> Sys.remove file);
    file
  with Sys_error e ->
    fatal_error 1 "while creating temp file: %s" e

let exec fmt =
  ksprintf (fun cmd ->
      printf "%s %s\n" (yellow_s "=>") cmd;
      let i = Sys.command cmd in
      if i <> 0 then fatal_error i "`%s' exited with code %d" cmd i
    ) fmt

let exec_output fmt =
  ksprintf (fun cmd ->
      let file = temp () in
      exec "%s > %s" cmd file;
      read file
    ) fmt
