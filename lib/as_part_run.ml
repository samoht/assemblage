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


(* Checks *)

let check p =
  let run = As_part.coerce `Run p in
  As_log.warn "%a part check is TODO" As_part.pp_kind (As_part.kind run);
  As_conf.true_

(* Actions *)

let actions ?dir action _ =
  let actions action cd dir = match dir with
  | None -> [ action ]
  | Some dir -> [ As_action.add_cmds `Before [ cd dir ] action ]
  in
  As_conf.(const actions $ action $ As_acmd.cd $ Option.wrap dir)

(* Run *)

let v ?usage ?exists ?args ?dir name action =
  let actions = actions ?dir action in
  As_part.v_kind ?usage ?exists ?args ~actions ~check name `Run

let with_bin ?usage ?exists ?args ?dir ?name ?ext bin cmds =
  let name = match name with None -> As_part.name bin | Some n -> n in
  let exists = match exists with
  | None -> As_part_bin.exists ?ext bin
  | Some exists -> As_conf.(exists &&& As_part_bin.exists ?ext bin)
  in
  let cpath = As_part_bin.to_cmd_path ?ext bin in
  let bin = As_part_bin.to_cmd ?ext bin in
  let action path bin cmds = As_action.v ~inputs:[ path ] (cmds bin) in
  v ?usage ~exists ?args ?dir name As_conf.(const action $ cpath $ bin $ cmds)

let bin ?usage ?exists ?args ?dir ?name ?ext ?stdin ?stdout ?stderr bin cargs =
  let cmds stdin stdout stderr cargs bin =
    [ As_acmd.v bin cargs ?stdin ?stdout ?stderr ]
  in
  let cmds =
    As_conf.(const cmds $ Option.wrap stdin $ Option.wrap stdout $
             Option.wrap stderr $ cargs)
  in
  with_bin ?usage ?exists ?args ?dir ?name ?ext bin cmds
