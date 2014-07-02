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

type shell = {
  dir : string;
  prog: string;
  args: string list;
}

type bash = {
  bash_dir: string;
  bash    : string;
}

type t =
  | Func of (unit -> unit)
  | Shell of shell
  | Bash of bash

let shell ~dir prog args =
  Shell { dir; prog; args }

let func fn =
  Func fn

let bash ~dir fmt =
  ksprintf (fun bash ->
      let bash_dir = dir in
      Bash { bash_dir; bash }
    ) fmt

let run = function
  | Func fn -> fn ()
  | Shell s ->
    let args = String.concat " " (s.prog :: s.args) in
    Shell.in_dir s.dir (fun () -> Shell.exec "%s" args)
  | Bash b  ->
    Shell.in_dir b.bash_dir (fun () -> Shell.exec "%s" b.bash)
