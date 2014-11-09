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

(* Part kinds *)

type kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo ]

let pp_kind ppf k = As_fmt.pp_str ppf begin match k with
  | `Base -> "base" | `Unit -> "unit" | `Lib -> "lib" | `Bin -> "bin"
  | `Pkg -> "pkg" | `Run -> "run" | `Doc -> "doc" | `Silo -> "silo"
  | `Dir -> "dir"
  end

let str_of_kind = Format.asprintf "%a" pp_kind

(* Usage *)

type usage = [ `Dev | `Test | `Build | `Doc | `Outcome | `Other of string ]

let pp_usage ppf u = As_fmt.pp_str ppf begin match u with
  | `Dev -> "dev" | `Test -> "test" | `Build -> "build" | `Doc -> "doc"
  | `Outcome -> "outcome" | `Other s -> s
  end

(* Metadata *)

type meta = As_univ.t
let meta_key = As_univ.create
let meta_nil = fst (meta_key ()) ()

(* Part *)

type +'a t =
  { id : int;
    kind : kind;
    name : string;
    usage : usage;
    cond : bool As_conf.value;
    meta : meta;
    needs : kind t list; (* N.B. parts unique but ordered. *)
    args : kind t -> As_args.t;
    actions : kind t -> As_action.t list;
    check : kind t -> bool; }
  constraint 'a = [< kind ]

let part_id =
  let count = ref (-1) in
  fun () -> incr count; !count

let uniq ps =                     (* uniquify part list while keeping order. *)
  let module Int = struct
    type t = int
    let compare : int -> int -> int = compare
  end in
  let module Set = Set.Make (Int) in
  let add (seen, ps as acc) p =
    if Set.mem p.id seen then acc else (Set.add p.id seen), (p :: ps)
  in
  List.rev (snd (List.fold_left add (Set.empty, []) ps))

let v_kind ?(usage = `Outcome) ?(cond = As_conf.true_) ?(meta = meta_nil)
    ?(needs = []) ?(args = fun _ -> As_args.empty)
    ?(actions = fun _ -> []) ?(check = fun _ -> true) name kind =
  let needs = uniq needs in
  { id = part_id ();
    kind = (kind :> kind);
    name; usage; cond; meta;
    needs = (needs :> kind t list);
    args;
    actions = (actions :> kind t -> As_action.t list);
    check = (check :> kind t -> bool); }

let v ?usage ?cond ?meta ?needs ?args ?actions ?check name =
  v_kind ?usage ?cond ?meta ?needs ?args ?actions ?check name `Base

let kind p = p.kind
let name p = p.name
let usage p = p.usage
let cond p = p.cond
let meta p = p.meta
let get_meta proj p =  match proj p.meta with
| None -> assert false | Some m -> m

let needs p = p.needs
let args p = p.args (p :> kind t)   (* FIXME memoize *)
let actions p = p.actions (p :> kind t) (* FIXME memoize *)
let check p = p.check (p :> kind t)

let products p =
  (* FIXME As_action doesn't thread condition, so this is not
     accurate according to config and we should maybe also
     directly thread p.cond *)
  let outputs = List.map As_action.outputs (actions p) in
  let add acc outputs = As_conf.(const List.rev_append $ outputs $ acc) in
  let rev_outputs = List.fold_left add (As_conf.const []) outputs in
  As_conf.(const List.rev $ rev_outputs)

let id p = p.id
let sid p = Format.asprintf "%a-%s" pp_kind p.kind p.name
let equal p p' = p.id = p'.id
let compare p p' = (compare : int -> int -> int) p.id p'.id
let with_kind_meta (#kind as k) meta p = { p with kind = k; meta; }

(* Coercing *)

let coerce (#kind as k) ({kind} as p) =
  if p.kind = k then p else
  invalid_arg (err_coerce (str_of_kind p.kind) (str_of_kind k))

let coerce_if (#kind as k) ({kind} as p) =
  if p.kind = k then Some p else None

(* Part lists *)

let keep pred ps =
  let keep acc p = if pred p then p :: acc else acc in
  List.rev (List.fold_left keep [] ps)

let keep_map fn ps =
  let add acc p = match fn p with None -> acc | Some v -> v :: acc in
  List.rev (List.fold_left add [] ps)

let keep_kind kind ps = keep_map (coerce_if kind) ps
let keep_kinds kinds ps = keep (fun p -> List.mem (kind p) kinds) ps

(* Part sets and maps *)

module Part = struct
  type part = kind t
  type t = part
  let compare = compare
end

module Set = struct
  include Set.Make (Part)
  let of_list = List.fold_left (fun acc s -> add s acc) empty
end

module Map = Map.Make (Part)

(* Fold *)

let fold_rec f acc l =
  let rec loop (seen, r as acc) = function
  | (next :: todo) :: todo' ->
      if Set.mem next seen then loop acc (todo :: todo') else
      loop (Set.add next seen, f r next) ((needs next) :: todo :: todo')
  | [] :: [] -> r
  | [] :: todo -> loop acc todo
  | [] -> assert false
  in
  loop (Set.empty, acc) [l]
