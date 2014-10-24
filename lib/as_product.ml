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

(* Build products *)

type t = [ `File of As_path.rel
         | `Effect of string * As_path.rel ] * bool As_conf.value

let cond (_, f) = f
let target = function `File p, _ -> As_path.to_string p | `Effect (n, _), _ -> n
let path = function (`File p | `Effect (_, p)), _ -> p
let raw_path p = As_path.to_string (path p)
let basename p = As_path.basename (path p)
let dirname = function
| `File p, _ -> As_path.(as_rel (dirname p))
| `Effect (_, p), _ -> p

(* Predicates *)

let is_file = function `File _, _ -> true | _ -> false
let is_effect = function `Effect _, _ -> true | _ -> false

let has_ext ext = function
| `File p, _ when As_path.has_ext ext p -> true
| _ -> false

let keep_ext ext = function
| `File p, _ as product when As_path.has_ext ext p -> Some product
| _ -> None

(* Converting to arguments *)

let target_to_args ?(pre = []) ctxs p =
  let args = pre @ [ target p ] in
  let cond = cond p in
  let add_ctx acc ctx = As_args.(acc @@@ create ~cond ctx args) in
  List.fold_left add_ctx As_args.empty ctxs

let dirname_to_args ?(pre = []) ctxs p =
  let args = pre @ [ As_path.to_string (dirname p) ] in
  let cond = cond p in
  let add_ctx acc ctx = As_args.(acc @@@ create ~cond ctx args) in
  List.fold_left add_ctx As_args.empty ctxs
