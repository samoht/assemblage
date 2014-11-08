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
module Path = As_path
module Log = As_log
module Cmd = As_cmd

(* Building *)

module Conf = As_conf
module Ctx = As_ctx
type args = As_args.t
module Args = As_args

module Action = struct
  include As_action
  module OCaml = As_action_ocaml
end

(* Parts *)

module Part = struct
  include As_part
  type part_kind = kind
  module Bin = As_part_bin
  module Custom = As_part_custom
  module Dir = As_part_dir
  module Doc = As_part_doc
  module Lib = As_part_lib
  module Pkg = As_part_pkg
  module Run = As_part_run
  module Silo = As_part_silo
  module Unit = As_part_unit
end

type path = string list
let ( / ) segs seg = List.rev (seg :: List.rev segs)

type part_kind = As_part.kind
type +'a part = 'a As_part.t

let unit ?cond ?args ?deps ?(kind = `OCaml (`Both, `Normal)) ?(dir = []) name =
  let src_dir = As_path.rel_of_segs dir in
  Part.Unit.create ?cond ?args ?deps name kind ~src_dir

let lib ?cond ?args ?deps ?byte ?native ?native_dynlink ?(kind = `OCaml) name
    units =
  As_part_lib.create ?cond ?args ?deps ?byte ?native ?native_dynlink
    name kind units

let bin ?cond ?args ?deps ?byte ?native ?js ?(kind = `OCaml) name units =
  As_part_bin.create ?cond ?args ?deps ?byte ?native ?js name kind units

let pkg ?cond ?args ?(kind = `OCaml `OCamlfind) name =
  As_part_pkg.create ?cond ?args name kind

let run ?cond ?args ?deps ?(dir = []) name cmds =
  let dir = As_path.rel_of_segs dir in
  As_part_run.create ?cond ?args ?deps ~dir name cmds

let doc ?cond ?args ?keep ?kind name parts =
  As_part_doc.create ?cond ?args ?keep ?kind name parts

let dir ?cond ?keep ?install kind =
  As_part_dir.create ?cond ?keep ?install kind

let silo ?cond ?args name parts =
  As_part_silo.create ?cond ?args name parts

(* Projects *)

module Project = As_project
type project = Project.t
let assemble = Project.assemble

module Private = struct
  module Fmt = As_fmt
  module Log = As_log
  module Cmd = As_cmd
  module Conf = As_conf
  module Action = struct
    include As_action
    module OCaml = As_action_ocaml
  end
  module Project = As_project
end
