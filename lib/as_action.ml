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

(* Actions *)

type t =
  { ctx : As_ctx.t;                         (* context to use on evaluation. *)
    inputs : As_path.t list; (* inputs that need to exist and be up to date. *)
    outputs : As_path.t list;    (* outputs that need to be touched by cmds. *)
    cmds : As_acmd.t list;                               (* action commands. *)
    args : As_args.t;               (* argument bundle to use on evaluation. *)
    log : string option; }    (* a high-level logging string for the action. *)

let v ?log ?(ctx = As_ctx.empty) ?(inputs = []) ?(outputs = []) cmds =
  { args = As_args.empty; log; ctx; inputs; outputs; cmds }

let ctx a = a.ctx
let inputs a = a.inputs
let outputs a = a.outputs
let cmds a = a.cmds
let args a = a.args
let log a = a.log
let products a = List.(rev_append (rev (inputs a)) (outputs a))

let add_ctx_args ctx args a =
  { a with ctx = As_ctx.union ctx a.ctx; args = As_args.append args a.args }

let pp conf ppf a =
  As_fmt.pp ppf
    "@[<v>    ctx: @[%a@]@, inputs: @[%a@]@,outputs: @[%a@]@,   cmds: @[%a@]\
     @,   args: @[%a@]@,    log: %a@]"
    As_ctx.pp a.ctx
    As_fmt.(pp_list ~pp_sep:pp_sp As_path.pp) a.inputs
    As_fmt.(pp_list ~pp_sep:pp_sp As_path.pp) a.outputs
    As_fmt.(pp_list ~pp_sep:pp_cut As_acmd.pp) a.cmds
    (As_args.pp conf) a.args
    As_fmt.(pp_opt pp_str) a.log

(* Action lists *)

let list_field field acc acts =
  let add_action acc a = List.rev_append (field a) acc in
  List.rev (List.fold_left add_action acc acts)

let list_inputs acts = list_field inputs [] acts
let list_outputs acts = list_field outputs [] acts
let list_products acts = list_field inputs (list_field outputs [] acts) acts

(* Build actions *)

let symlink =
  let action ln_rel src dst =
    v ~ctx:As_ctx.empty ~inputs:[src] ~outputs:[dst] [ln_rel src dst]
  in
  As_conf.(const action $ As_acmd.ln_rel)
