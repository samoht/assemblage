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

let read file =
  if Sys.file_exists file then
    let ic = open_in_bin file in
    Some (input_line ic)
  else
    None

let (/) = Filename.concat

let git file = ".git" / file

let head () =
  match read (git "HEAD") with
  | None   -> None
  | Some s ->
    let reference =
      try
        let c = String.rindex s ' ' in
        String.sub s (c+1) (String.length s -c-1)
      with Not_found ->
        s in
    match read (git reference) with
    | None      -> None
    | Some sha1 -> Some sha1

let describe ?(chop_v=false) () =
  if Asd_shell.try_exec "git describe --always" then
    match Asd_shell.exec_output "git describe --always" with
    | d::_ ->
      let len = String.length d in
      if chop_v && len > 0 && d.[0] = 'v' then
        Some (String.sub d 1 (len - 2))
      else
        Some (String.sub d 0 (len - 1)) (* remove \n *)
    | _ -> None
  else
    None
