(*
 * Copyright (c) 2015 Daniel C. Bünzli
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

open Astring
open Rresult
open Bos

type t = [ `Git | `Hg ]

let dirtify id = id ^ "-dirty"

(* VCS detection and executable override *)

let override_kind = ref None
let override_exec = ref None
let set_override_kind v = override_kind := v
let set_override_exec exec = override_exec := exec
let override_kind () = !override_kind
let override_exec () = !override_exec

(* Git *)

let git_dir = ".git"
let git_exists root_dir = match override_kind () with
| Some `Git -> Ok true
| Some _ | None -> OS.Dir.exists Path.(root_dir / git_dir)

let git root_dir args =
  let git = match override_exec () with
  | Some exec when override_kind () = Some `Git -> exec
  | _ -> "git"
  in
  let dir = Path.(to_string (root_dir / git_dir)) in
  OS.Cmd.exec_read git ("--git-dir" :: dir :: args)

let git_head mark_dirty root_dir =
  git root_dir [ "show-ref"; "HEAD"; "--hash" ] >>= fun hash ->
  if not mark_dirty then Ok hash else
  git root_dir [ "status"; "--porcelain" ] >>= function
  | "" -> Ok hash
  | _ -> Ok (dirtify hash)

let git_describe mark_dirty root_dir =
  let opt = if mark_dirty then [ "--dirty" ] else [] in
  git root_dir ([ "describe"; "--always"; ] @ opt)

(* Hg *)

let hg_dir = ".hg"
let hg_exists root_dir = match override_kind () with
| Some `Hg -> Ok true
| Some _ | None  -> OS.Dir.exists Path.(root_dir / hg_dir)

let hg root_dir args =
  let hg = match override_exec () with
  | Some exec when override_kind () = Some `Hg -> exec
  | _ -> "hg"
  in
  OS.Cmd.exec_read hg ("--repository" :: Path.to_string root_dir :: args)

let hg_id root_dir =
  hg root_dir [ "id"; "-i" ] >>= fun id ->
  let is_dirty = String.length id > 0 && id.[String.length id - 1] = '+' in
  let id = if is_dirty then String.slice ~stop:(-1) id else id in
  Ok (id, is_dirty)

let hg_head mark_dirty root_dir =
  hg_id root_dir >>= fun (id, is_dirty) ->
  Ok (if is_dirty && mark_dirty then dirtify id else id)

let hg_describe mark_dirty root_dir =
  let get_distance s = try Ok (int_of_string s) with
  | Failure _ -> R.error_msg "could not parse hg tag distance"
  in
  let hg_parent template = hg root_dir [ "parent"; "--template"; template ] in
  hg_parent "\"{latesttagdistance}\"" >>= get_distance
  >>= begin function
  | 1 -> hg_parent "\"{latesttag}\""
  | n -> hg_parent "\"{latesttag}-{latesttagdistance}-{node|short}\""
  end
  >>= fun descr ->
  if not mark_dirty then Ok descr else
  hg_id root_dir >>= fun (_, is_dirty) ->
  Ok (if is_dirty then dirtify descr else descr)

(* VCS detection *)

let exists root_dir = function
| `Git -> git_exists root_dir
| `Hg -> hg_exists root_dir

let find root_dir = match override_kind () with
| Some kind -> Ok (Some kind)
| None ->
    git_exists root_dir >>= function
    | true -> Ok (Some `Git)
    | false ->
        hg_exists root_dir >>= function
        | true -> Ok (Some `Hg)
        | false -> Ok None

let get root_dir =
  find root_dir >>= function
  | Some vcs -> Ok vcs
  | None -> R.error_msg "No VCS found"

(* VCS commands *)

let head ?(dirty = true) root_dir = function
| `Git -> git_head dirty root_dir
| `Hg -> hg_head dirty root_dir

let describe ?(dirty = true) root_dir = function
| `Git -> git_describe dirty root_dir
| `Hg -> hg_describe dirty root_dir
