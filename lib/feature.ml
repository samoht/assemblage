(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf
open Cmdliner

type t = {
  name: string;
  default: bool;
  doc: string;
}

type formula =
  | True | False
  | Atom of t
  | Not of formula
  | And of formula * formula
  | Or of formula * formula

type cnf =
  [ `Conflict | `And of [ `P of t | `N of t ] list ]

let atom t = Atom t

module Set = Set.Make(struct
    type s = t
    type t = s
    let compare x y = String.compare x.name y.name
  end)

let (++) = Set.union

let atoms t =
  let set = ref Set.empty in
  let rec aux = function
    | True
    | False     -> ()
    | Atom t    -> set := Set.add t !set
    | Not x     -> aux x
    | And (x, y)
    | Or (x, y) -> aux x; aux y in
  aux t;
  !set

let negate: cnf -> cnf = function
  | `Conflict -> `And []
  | `And []   -> `Conflict
  | `And l    -> `And (List.map (function `P t -> `N t | `N t -> `P t) l)

let (@) (x:cnf) (y:cnf) = match x, y with
  | `Conflict , _ | _, `Conflict -> `Conflict
  | `And []   , x | x, `And []   -> x
  | `And x , `And y  ->
    let p = Hashtbl.create (List.length x + List.length y) in
    let n = Hashtbl.create (List.length x + List.length y) in
    let add = function
      | `P x -> Hashtbl.replace p x true
      | `N x -> Hashtbl.replace n x true in
    List.iter add x;
    List.iter add y;
    try
      let ps =
        Hashtbl.fold (fun p _ acc ->
            if Hashtbl.mem n p then raise Exit
            else `P p :: acc
          ) p [] in
      let ns = Hashtbl.fold (fun n _ acc ->
          if Hashtbl.mem p n then raise Exit
          else `N n :: acc
        ) n [] in
      `And (ps @ ns)
    with Exit ->
      `Conflict

let rec cnf: formula -> cnf = function
  | True       -> `And []
  | False      -> `Conflict
  | Atom x     -> `And [`P x]
  | Not x      -> negate (cnf x)
  | And (x, y) -> cnf x @ cnf y
  | Or (x, y)  -> cnf (Not x) @ cnf (Not y)

let rec eval tbl = function
  | True       -> true
  | False      -> false
  | Atom t     -> (try List.assoc t tbl with Not_found -> false)
  | Not f      -> not (eval tbl f)
  | And (x, y) -> (eval tbl x) && (eval tbl y)
  | Or (x, y)  -> (eval tbl x) || (eval tbl y)

let not f = Not f

let true_ = True

let false_ = False

let (&&) x y = And (x, y)

let (||) x y = Or (x, y)

let name t = t.name

let default t = t.default

let with_default t default =
  { t with default }

let create ~doc ~default name = { name; default; doc }

let parse t =
  let default = if t.default then "$(b,enable)" else "$(b,disable)" in
  let enable =
    let d = Arg.info ~doc:(sprintf "Enable %s (default is %s)." t.doc default)
        ["enable-" ^ t.name] in
    Arg.(value & flag & d) in
  let disable =
    let d = Arg.info ~doc:(sprintf "Disable %s (default is %s)." t.doc default)
        ["disable-" ^ t.name] in
    Arg.(value & flag & d) in
  let create enable disable =
    let v = match enable, disable with
      | true , false -> true
      | false, true  -> false
      | false, false -> t.default
      | true , true  -> failwith "Invalid flag" in
    (t, v) in
  Term.(pure create $ enable $ disable)

let native_t =
  create ~doc:"native code compilation." ~default:true "native"

let native_dynlink_t =
  create ~doc:"native plugins for native code." ~default:true "native-dynlink"

let annot_t =
  create ~doc:"generation of binary annotations." ~default:true "annot"

let debug_t =
  create ~doc:"generation of debug symbols." ~default:true "debug"

let warn_error_t =
  create ~doc:"warning as errors." ~default:false "warn-error"

let test_t =
  create ~doc:"tests." ~default:false "test"

let doc_t =
  create ~doc:"the generation of documentation." ~default:true "doc"

let js_t =
  create ~doc:"the generation of JavaScript build artefacts, using `js_of_ocaml'."
    ~default:false "js"

let base = List.fold_left (fun set t -> Set.add t set) Set.empty [
    native_t; native_dynlink_t;
    debug_t; annot_t; warn_error_t;
    test_t; doc_t; js_t;
  ]

let native = atom native_t
let native_dynlink = atom native_dynlink_t
let annot = atom annot_t
let warn_error = atom warn_error_t
let debug = atom debug_t
let test = atom test_t
let doc = atom doc_t
let js = atom js_t
