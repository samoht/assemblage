(*
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

(* Products *)

type product = As_path.rel As_conf.value
type products = As_path.rel list As_conf.value

(* Build commands *)

type cmd =
  { exec : string As_conf.key;
    args : string list As_conf.value;
    stdin : As_path.rel As_conf.value option;
    stdout : As_path.rel As_conf.value option;
    stderr : As_path.rel As_conf.value option; }

type cmds = cmd list

let cmd ?stdin ?stdout ?stderr exec args =
  [{ exec; args; stdin; stdout; stderr }]

let seq cmds cmds' =  List.rev_append (List.rev cmds) cmds'
let (<*>) = seq

(* Actions *)

type t =
  { cond : bool As_conf.value;
    ctx : As_ctx.t;
    inputs : As_path.rel list As_conf.value;
    outputs : As_path.rel list As_conf.value;
    cmds : cmds; }

let v ?(cond = As_conf.true_) ~ctx ~inputs ~outputs cmds =
  { cond; ctx; inputs; outputs; cmds }

let cond r = r.cond
let ctx r = r.ctx
let inputs r = r.inputs
let outputs r = r.outputs
let cmds r = r.cmds

module Spec = struct

  (* List configuration values *)

  type 'a list_v = 'a list As_conf.value

  let atom v = As_conf.(const [v])
  let atoms v = As_conf.(const v)

  let addl l l' = List.rev_append (List.rev l) l'
  let addl_if c l l' = if c then addl l l' else l'

  let add l l' = As_conf.(const addl $ l $ l')
  let add_if c l l' = As_conf.(const addl_if $ c $ l $ l')
  let add_if_key c l l' = add_if (As_conf.value c) l l'

  (* Path and products *)

  let path p ~ext:e =
    let change_ext p = As_path.(as_rel (change_ext p e)) in
    As_conf.(const change_ext $ p)

  let path_base p = As_conf.(const As_path.basename $ p)
  let path_dir p = As_conf.(const (fun p -> As_path.(as_rel (dirname p))) $ p)
  let path_arg ?opt p =
    let make_arg p =
      let p = As_path.to_string p in
      match opt with None -> [p] | Some opt -> [opt; p]
    in
    As_conf.(const make_arg $ p)

  let paths_args ?opt ps =
    let make_args ps =
      let add = match opt with
      | None -> fun acc p -> As_path.to_string p :: acc
      | Some opt -> fun acc p -> As_path.to_string p :: opt :: acc
      in
      List.rev (List.fold_left add [] ps)
    in
    As_conf.(const make_args $ ps)

  let product ?ext p =
    let p = match ext with None -> p | Some ext -> path p ~ext in
    As_conf.(const (fun p -> [p]) $ p)

  (* Commands *)
  let ( <*> ) = ( <*> )
end
