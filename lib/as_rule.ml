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

type cmd = As_args.t * (string list -> string list)
let cmd args = As_args.empty, (fun _ -> args)

type action = cmd list

type t =
  { context : As_context.t;
    inputs : As_product.t list;
    outputs : As_product.t list;
    action : action; }

let create ~context ~inputs ~outputs ~action =
  let inputs_conds = List.map snd inputs in
  let adjust (t, cond) = t, List.fold_left As_cond.(&&&) cond inputs_conds in
  let outputs = List.map adjust outputs in
  { context; inputs; outputs; action }

let context r = r.context
let inputs r = r.inputs
let outputs r = r.outputs
let action r = r.action

(* Predicates *)

let has_context ctx r = r.context = ctx

(* Built-in rules *)

let link ?(cond = As_cond.true_) ?(args = As_args.empty) env ~src ~dst =
  let dst_dir = As_path.(as_rel (dirname dst)) in
  let inputs = [ `File src, cond; `File dst_dir, cond] in
  let outputs = [ `File dst, cond ] in
  let abs_src = As_path.(to_string (As_env.root_dir env // src)) in
  let abs_dst = As_path.(to_string (As_env.root_dir env // dst)) in
  let link args = As_env.ln env :: args @ [ abs_src; abs_dst ] in
  let action = [args, link] in
  create ~context:(`Other "link") ~inputs ~outputs ~action

let mkdir ?(cond = As_cond.true_) ?(args = As_args.empty) env ~dir:d =
  let inputs = [] in
  let outputs = [ `File d, cond ] in
  let mkdir args = As_env.mkdir env :: args @ [As_path.to_string d] in
  let action = [args, mkdir] in
  create ~context:(`Other "mkdir") ~inputs ~outputs ~action
