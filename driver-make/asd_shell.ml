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

open Assemblage

let str = Printf.sprintf
let null = match Sys.os_type with "Win32" -> " NUL" | _ -> "/dev/null"

let fatal_error i fmt =
  let exit () = exit i in
  Log.(kmsg exit Error fmt)

(* Execute commands *)

let verbose_default = ref false

let has_cmd cmd =
  let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
  Sys.command (str "%s %s 1>%s 2>%s" test cmd null null) = 0

let read file =
  try
    let ic = open_in file in
    let lines =
      let lines = ref [] in
      try
        while true do lines := input_line ic :: !lines done;
        assert false
      with End_of_file -> List.rev !lines
    in
    close_in ic;
    lines
  with Sys_error e -> fatal_error 1 "%s: %s" file e

let temp () =
  try
    let file = Filename.temp_file (Filename.basename Sys.argv.(0)) ".out" in
    at_exit (fun () -> try Sys.remove file with Sys_error e -> ());
    file
  with Sys_error e -> fatal_error 1 "creating@ temporary@ file: %s" e

let exec ?verbose fmt =
  let verbose = match verbose with None -> !verbose_default | Some v -> v in
  let run cmd =
    if verbose
    then Log.show "%a@ %s\n" (Fmt.pp_styled_str `Yellow) "-->" cmd;
    let i = Sys.command cmd in
    if i <> 0 then fatal_error i "`%s'@ exited@ with@ code %d" cmd i
  in
  Printf.ksprintf run fmt

let try_exec fmt =
  let try_run cmd = (Sys.command (str "%s 1>%s 2>%s" cmd null null)) = 0 in
  Printf.ksprintf try_run fmt

let exec_output ?verbose fmt =
  let run_read cmd =
    let file = temp () in
    exec ?verbose "%s 1> %s" cmd file;
    read file
  in
  Printf.ksprintf run_read fmt

let in_dir dir fn =
  let last_cwd = Sys.getcwd () in
  try
    if not (Sys.file_exists dir && Sys.is_directory dir)
    then failwith (str "%s does not exist" dir)
    else begin
      Sys.chdir dir;
      let r = fn () in
      Sys.chdir last_cwd;
      r
    end
  with e -> Sys.chdir last_cwd; raise e
