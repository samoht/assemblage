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

(* Metadata *)

type meta = { dir : As_path.t As_conf.value }
let inj, proj = As_part.meta_key ()
let get_meta p = As_part.get_meta proj p
let meta ?(dir = As_conf.(value root_dir)) () = inj { dir }
let dir p = (get_meta p).dir

(* Run *)

let v ?usage ?cond ?(args = As_args.empty) ?dir name cmds =
  let meta = meta ?dir () in
  let args _ = args in
  As_part.v_kind ?usage ?cond ~meta ~args name `Run


let of_bin ?usage ?cond ?args ?dir bin cmds = failwith "TODO"

let of_base ?dir p =
  let meta = meta ?dir () in
  As_part.with_kind_meta `Run meta p
