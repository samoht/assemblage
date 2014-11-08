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

let str = Printf.sprintf
let err_coerce k exp = str "part has kind %s not %s" k exp

type meta = As_univ.t
let meta_key = As_univ.create

type kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo | `Custom ]

let kind_to_string = function
| `Base -> "base" | `Unit -> "unit" | `Lib -> "lib" | `Bin -> "bin"
| `Pkg -> "pkg" | `Run -> "run" | `Doc -> "doc" | `Silo -> "silo"
| `Dir -> "dir" | `Custom -> "custom"

type +'a t =
  { kind : kind;
    name : string;
    cond : bool As_conf.value;
    deps : kind t list;
    args : kind t -> As_args.t;
    actions : kind t -> As_action.t list;
    meta : meta; }
constraint 'a = [< kind ]

(* Comparing *)

let id p = kind_to_string p.kind ^ p.name
let equal p p' = id p = id p'
let compare p p' = Pervasives.compare (id p) (id p')

let to_set l =
  let rec add seen acc = function
  | [] -> List.rev acc
  | p :: ps ->
      let name = id p in
      if As_string.Set.mem name seen then add seen acc ps else
      add (As_string.Set.add name seen) (p :: acc) ps
  in
  add As_string.Set.empty [] l

let create ?(cond = As_conf.true_) ?(args = fun _ -> As_args.empty)
    ?(deps = []) ?(actions = fun _  -> []) name kind meta =
  let deps = to_set deps in
  { kind = (kind :> kind);
    name = name;
    cond = cond;
    deps = (deps :> kind t list);
    actions = (actions :> kind t -> As_action.t list);
    args = args;
    meta = meta }

(* Basic fields *)

let name p = p.name
let kind p = p.kind
let cond p = p.cond
let args p = p.args (p :> kind t)   (* FIXME memoize *)
let deps p = p.deps
let actions p = p.actions (p :> kind t) (* FIXME memoize *)
let meta p = p.meta
let get_meta proj p =  match proj p.meta with
| None -> assert false | Some m -> m

let products p =
  let outputs = List.map As_action.outputs (actions p) in
  let add acc outputs = As_conf.(const List.rev_append $ outputs $ acc) in
  let rev_outputs = List.fold_left add (As_conf.const []) outputs in
  As_conf.(const List.rev $ rev_outputs)

(* Coercing *)

let coerce (#kind as k) ({kind} as p) =
  if p.kind = k then p else
  invalid_arg (err_coerce (kind_to_string p.kind) (kind_to_string k))

let coerce_if (#kind as k) ({kind} as p) = if p.kind = k then Some p else None

(* Component list operations *)

let keep pred ps =
  let keep acc p = if pred p then p :: acc else acc in
  List.rev (List.fold_left keep [] ps)

let keep_map fn ps =
  let add acc p = match fn p with None -> acc | Some v -> v :: acc in
  List.rev (List.fold_left add [] ps)

let keep_kind kind ps = keep_map (coerce_if kind) ps
let keep_kinds kinds ps = keep (fun p -> List.mem (kind p) kinds) ps


let add_deps_args deps args u = (* N.B. this slows downs things a lot. *)
  let deps = to_set (u.deps @ deps) in
  let args _ = As_args.(@@@) (u.args (u :> kind t)) args in
  { u with deps; args }
