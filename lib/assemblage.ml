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

(* Pereliminaries *)

include Astring

module Fmt = Fmt
module Path = As_path
module Log = As_log
module Cmd = As_cmd

(* Building *)

module Conf = As_conf
module Ctx = As_ctx
module Args = As_args
module Acmd = As_acmd
module Action = As_action

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
let ( // ) p rel = Conf.(const Path.concat $ p $ rel)

let unit ?usage ?exists ?args ?needs ?(kind = `OCaml (`Both, `Normal))
    ?dir name =
  Unit.v ?usage ?exists ?args ?needs ?dir name kind

let lib ?usage ?exists ?args ?byte ?native ?native_dynlink ?(kind = `OCaml)
    name needs =
  Lib.v ?usage ?exists ?args ?byte ?native ?native_dynlink name kind needs

let bin ?usage ?exists ?args ?byte ?native ?js ?(kind = `OCaml) name needs =
  Bin.v ?usage ?exists ?args ?byte ?native ?js name kind needs

let pkg ?usage ?exists ?opt ?(kind = `OCamlfind) name =
  Pkg.v ?usage ?exists ?opt name kind

let doc ?usage ?exists ?args ?keep ?(kind = `OCamldoc) name needs =
  Doc.v ?usage ?exists ?args ?keep name kind needs

let dir ?usage ?exists ?args ?spec ?install kind needs =
  Dir.v ?usage ?exists ?args ?spec ?install kind needs

let file ?usage ?exists p =
  Part.file ?usage ?exists p

let run ?usage ?exists ?args ?dir name action =
  Run.v ?usage ?exists ?args ?dir name action

(* Projects *)

module Project = As_project
type project = Project.t
let assemble = Project.assemble

(* Private API *)

module Private = struct
  module Log = As_log
  module Cmd = As_cmd
  module Conf = As_conf
  module Args = As_args
  module Acmd = struct
    type args = Args.t
    include Acmd
  end
  module Action = As_action
  module Part = As_part
  module Project = As_project
end
