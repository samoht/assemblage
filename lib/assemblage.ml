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
module Args = As_args
module Acmd = As_acmd
module Action = struct
  include As_action
  module OCaml = As_action_ocaml
end

(* Parts *)

module Part = As_part
module Unit = As_part_unit
module Lib = As_part_lib
module Bin = As_part_bin
module Pkg = As_part_pkg
module Doc = As_part_doc
module Dir = As_part_dir
module Run = As_part_run

type part_kind = As_part.kind
type +'a part = 'a As_part.t

(* Part specification combinators *)

type path = Path.t Conf.value

let root = Conf.(const As_path.empty)
let ( / ) p seg = Conf.(const Path.add $ p $ const seg)

let unit ?usage ?cond ?args ?needs ?(kind = `OCaml (`Both, `Normal)) ?dir name =
  Unit.v ?usage ?cond ?args ?needs ?dir name kind

let lib ?usage ?cond ?args ?byte ?native ?native_dynlink ?(kind = `OCaml) name
    needs =
  Lib.v ?usage ?cond ?args ?byte ?native ?native_dynlink name kind needs

let bin ?usage ?cond ?args ?byte ?native ?js ?(kind = `OCaml) name needs =
  Bin.v ?usage ?cond ?args ?byte ?native ?js name kind needs

let pkg ?usage ?cond ?args ?(kind = `OCaml `OCamlfind) name =
  Pkg.v ?usage ?cond ?args name kind

let doc ?usage ?cond ?args ?keep ?(kind = `OCamldoc) name needs =
  Doc.v ?usage ?cond ?args ?keep name kind needs

let dir ?usage ?cond ?args ?keep ?install kind needs =
  Dir.v ?usage ?cond ?args ?keep ?install kind needs

let file ?usage ?cond p =
  Part.file ?usage ?cond p

let run ?usage ?cond ?args ?dir name action =
  Run.v ?usage ?cond ?args ?dir name action

(* Projects *)

module Project = As_project
type project = Project.t
let assemble = Project.assemble

(* Private API *)

module Private = struct
  module Fmt = As_fmt
  module Log = As_log
  module Cmd = As_cmd
  module Conf = As_conf
  module Args = As_args
  module Acmd = struct
    type args = Args.t
    include Acmd
  end
  module Action = struct
    include As_action
    module OCaml = As_action_ocaml
  end
  module Part = As_part
  module Project = As_project
end
