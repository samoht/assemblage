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

let str = Format.asprintf

(* Part kinds *)

type kind = [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo ]
let pp_kind ppf k = As_ctx.pp_kind ppf (k :> kind) (* weird *)

let err_coerce k exp =
  Format.asprintf "part has kind %a not %a" pp_kind k pp_kind exp

(* Usage *)

type usage = [ `Dev | `Test | `Build | `Doc | `Outcome | `Other of string ]
let pp_usage = As_ctx.pp_usage

(* Metadata *)

type meta = { meta : As_univ.t; meta_deps : As_univ.t -> As_conf.Key.Set.t }

let meta_key deps =
  let inj, proj = As_univ.create () in
  let meta_deps m = match proj m with None -> assert false | Some m -> deps m in
  let inj meta = { meta = inj meta; meta_deps } in
  let proj m = proj m.meta in
  inj, proj

let meta_deps_none _ = As_conf.Key.Set.empty
let meta_nil = fst (meta_key meta_deps_none) ()

(* Part definition, sets and maps *)

type +'a t =
  { id : int;                                   (* a unique id for the part. *)
    kind : kind;                                         (* the part's kind. *)
    name : string;                      (* the part name, may not be unique. *)
    usage : usage;                                      (* the part's usage. *)
    cond : bool As_conf.value;             (* [true] if available in config. *)
    args : As_args.t;                           (* end user argument bundle. *)
    meta : meta;                         (* part's metadata (kind specific). *)
    needs : kind t list;          (* part's need, n.b. unique and *ordered*. *)
    root : As_path.rel As_conf.value;        (* part's build root directory. *)
    action_defs : kind t -> As_action.t list;          (* action definition. *)
    actions : As_action.t list Lazy.t; (* part's actions (via actions_defs). *)
    check : kind t -> bool; }               (* part's sanity check function. *)
  constraint 'a = [< kind ]

module Def = struct
  type part = kind t
  type t = part
  let compare p p' = (compare : int -> int -> int) p.id p'.id
end

module Set = struct
  include Set.Make (Def)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = Map.Make (Def)

(* Part *)

let part_id =
  let count = ref (-1) in
  fun () -> incr count; !count

let alloc_root =
  (* We intercept and resolve duplicate *default* roots at the lowest level.
     This is part of the strategy to avoid crazy graph rewriting operations. *)
  let allocated = ref As_string.Set.empty in
  fun kind usage name ->
    let part_root =
      let base = match usage with
      | `Outcome -> str "%a-%s" pp_kind kind name
      | u -> str "%a-%a-%s" pp_kind kind pp_usage u name
      in
      let candidate = ref base in
      try
        for i = 1 to max_int do
          if not (As_string.Set.mem !candidate !allocated) then raise Exit else
          candidate := str "%s~%d" base i
        done;
        As_log.warn "You are being unreasonable, consider yourself doomed.";
        !candidate
      with Exit ->
        allocated := As_string.Set.add !candidate !allocated;
        !candidate
    in
    let in_build_dir build = As_path.(as_rel (build / part_root)) in
    As_conf.(const in_build_dir $ (value As_conf.build_dir))

let list_uniq ps =               (* uniquify part list while keeping order. *)
  let add (seen, ps as acc) p =
    if Set.mem p seen then acc else (Set.add p seen), (p :: ps)
  in
  List.rev (snd (List.fold_left add (Set.empty, []) ps))

let ctx p =                                      (* a context for the part. *)
  As_ctx.(add (`Part (`Name p.name)) @@
          add ((`Part p.kind) :> As_ctx.elt) @@
          add ((`Part p.usage) :> As_ctx.elt) @@
          empty)

let compute_actions p = (* gets actions from defining fun, adds ctx and args *)
  let ctx = ctx p in
  let add acc a = (As_action.add_ctx_args ctx p.args a) :: acc in
  List.rev (List.fold_left add [] (p.action_defs p))

let v_kind ?(usage = `Outcome) ?(cond = As_conf.true_) ?(args = As_args.empty)
    ?(meta = meta_nil) ?(needs = []) ?root ?(actions = fun _ -> [])
    ?(check = fun _ -> true) name kind =
  (* Man it's coercion hell in there. *)
  let needs = list_uniq (needs :> Set.elt list) in
  let root = match root with
  | None -> alloc_root (kind :> kind) (usage :> usage) name
  | Some r -> r
  in
  let rec part =
    { id = part_id (); kind = (kind :> kind);
      name; usage = usage; cond; args;
      meta; needs = (needs :> kind t list); root;
      action_defs = (actions :> kind t -> As_action.t list);
      actions = lazy (compute_actions (part :> kind t));
      check = (check :> kind t -> bool); }
  in
  part

let v ?usage ?cond ?args ?meta ?needs ?root ?actions ?check name =
  v_kind ?usage ?cond ?args ?meta ?needs ?root ?actions ?check name `Base

let id p = p.id
let kind p = p.kind
let name p = p.name
let usage p = p.usage
let cond p = p.cond
let args p = p.args
let meta p = p.meta
let needs p = p.needs
let root p = p.root
let actions p = Lazy.force (p.actions)
let check p = p.check (p :> kind t)

let get_meta proj p =  match proj p.meta with
| None -> assert false | Some m -> m

let deps p =
  (* Note we don't add p.needs's deps. If they are really needed
     they will have propagated in our action defs, same for
     p.args/p.root. *)
  let union = As_conf.Key.Set.union in
  let add_action acc a = union acc (As_action.deps a) in
  List.fold_left add_action As_conf.Key.Set.empty (actions p)
  |> union (As_conf.deps p.cond)
  |> union (p.meta.meta_deps p.meta.meta)

(* FIXME As_action doesn't thread condition, so this is not
   accurate according to config and we should maybe also
   directly thread p.cond (likely no, review that again). *)
let products ?exts p =
  let action_outputs = match exts with
  | None -> As_action.outputs
  | Some es ->
      fun a -> As_conf.List.keep (As_path.ext_matches es) (As_action.outputs a)
  in
  let rev_outputs = List.rev_map action_outputs (actions p) in
  As_conf.List.(flatten (rev_wrap rev_outputs))

let equal p p' = p.id = p'.id
let compare = Def.compare

(* FIXME we should make it clear in the {Bin,Unit,...}.of_base part
   functions that their meta will be overwritten and that they should
   not try to access it in their def function (that function
   will get called again if there is a with_root call *)
let with_kind_meta (#kind as k) meta p = { p with kind = k; meta; }

(* Part root directory *)

let with_root root old =
  let rec newp =
    { old with root; actions = lazy (compute_actions (newp :> kind t)) }
  in
  newp

let rooted ?ext p name =
  let mk_file r = match ext with
  | None -> As_path.(r / name)
  | Some e -> As_path.(r / name + e)
  in
  As_conf.(const mk_file $ p.root)

(* Coercing *)

let coerce (#kind as k) ({kind} as p) =
  if p.kind = k then p else invalid_arg (err_coerce p.kind k)

let coerce_if (#kind as k) ({kind} as p) =
  if p.kind = k then Some p else None

(* Part lists *)

let list_products ?exts ps =
  let rev_products = List.rev_map (products ?exts) ps in
  As_conf.List.(flatten (rev_wrap rev_products))

let list_keep pred ps =
  let keep acc p = if pred p then p :: acc else acc in
  List.rev (List.fold_left keep [] ps)

let list_keep_map fn ps =
  let add acc p = match fn p with None -> acc | Some v -> v :: acc in
  List.rev (List.fold_left add [] ps)

let list_keep_kind kind ps = list_keep_map (coerce_if kind) ps
let list_keep_kinds kinds ps = list_keep (fun p -> List.mem (kind p) kinds) ps
let list_fold f acc ps = List.fold_left f acc ps
let list_fold_kind kind f acc ps =
  let f acc p = match coerce_if kind p with None -> acc | Some p -> f acc p in
  list_fold f acc ps

let list_fold_rec f acc ps =
  let rec loop (seen, r as acc) = function
  | (next :: todo) :: todo' ->
      if Set.mem next seen then loop acc (todo :: todo') else
      loop (Set.add next seen, f r next) ((needs next) :: todo :: todo')
  | [] :: [] -> r
  | [] :: todo -> loop acc todo
  | [] -> assert false
  in
  loop (Set.empty, acc) [ps]

let list_fold_kind_rec kind f acc ps =
  let f acc p = match coerce_if kind p with None -> acc | Some p -> f acc p in
  list_fold_rec f acc ps
