(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(* Perliminaries *)

module String = As_string
module Fmt = As_fmt
module Log = As_log
module Path = As_path
module Cmd = As_cmd

(* Building *)

module Cond = As_cond
module Conf = As_conf
module Context = As_context
module Args = As_args
module Env = As_env
module Product = As_product
module Rule = As_rule

type cond = As_cond.t
type context = As_context.t
type args = As_args.t
type env = As_env.t
type rule = As_rule.t

(* Parts *)

module Part = As_part

type path = string list
let ( / ) segs seg = List.rev (seg :: List.rev segs)

type part_kind = As_part.kind
type +'a part = 'a As_part.t

let unit ?cond ?args ?deps ?(kind = `OCaml (`Both, `Normal)) ?(dir = []) name =
  let src_dir = fun _ -> As_path.rel_of_segs dir in
  As_part.Unit.create ?cond ?args ?deps name kind ~src_dir

let lib ?cond ?args ?deps ?byte ?native ?native_dynlink ?(kind = `OCaml) name
    units =
  As_part.Lib.create ?cond ?args ?deps ?byte ?native ?native_dynlink
    name kind units

let bin ?cond ?args ?deps ?byte ?native ?js ?(kind = `OCaml) name units =
  As_part.Bin.create ?cond ?args ?deps ?byte ?native ?js name kind units

let pkg ?cond ?args ?(kind = `OCaml `OCamlfind) name =
  As_part.Pkg.create ?cond ?args name kind

let run ?cond ?args ?deps ?(dir = []) name cmds =
  let run_dir = As_path.rel_of_segs dir in
  As_part.Run.create ?cond ?args ?deps ~run_dir name cmds

let doc ?cond ?args ?deps ?keep ?kind name parts =
  As_part.Doc.create ?cond ?args ?deps ?keep ?kind name parts

let dir ?cond ?args ?deps ?keep ?install kind =
  As_part.Dir.create ?cond ?args ?deps ?keep ?install kind

let silo ?cond ?args ?deps name parts =
  As_part.Silo.create ?cond ?args ?deps name parts

(* Projects *)

module Project = As_project
type project = Project.t

let projects = ref []
let assemble p = projects  := p :: !projects

module Private = struct
  let projects () = !projects
  module Conf = As_conf
end
