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

type cmd_atom = [ `Cmd of string As_conf.key ] * string * string list
type cmd = cmd_atom list As_conf.value

let cmd exec args =
  let ctx_elt = [ `Cmd exec ] in
  let cmd ctx_elt exec args = [ctx_elt, exec, args] in
  As_conf.(const cmd $ const ctx_elt $ value exec $ args)

let seq cmd0 cmd1 =  As_conf.(const ( @ ) $ cmd0 $ cmd1)
let (<*>) = seq

type t =
  { cond : bool As_conf.value;
    ctx : As_ctx.t;
    args : As_args.t;
    inputs : As_product.t list As_conf.value;
    outputs : As_product.t list As_conf.value;
    cmd : cmd; }

let create ?(cond = As_conf.true_) ?(ctx = As_ctx.empty) ?(args = As_args.empty)
    ~inputs ~outputs cmd =
  { cond; ctx; args; inputs; outputs; cmd }

let cond r = r.cond
let ctx r = r.ctx
let args r = r.args
let inputs r = r.inputs
let outputs r = r.outputs
let cmd r = r.cmd
