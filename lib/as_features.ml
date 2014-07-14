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

type elt = {
  name: string;
  default: bool;
  doc: string;
}

let doc t = t.doc

type t =
  | True | False
  | Atom of elt
  | Not of t
  | And of t * t
  | Or of t * t

type cnf =
  [ `Conflict | `And of [ `P of elt | `N of elt ] list ]

let atom t = Atom t

module Set = Set.Make(struct
    type t = elt
    let compare x y = String.compare x.name y.name
  end)

type set = Set.t

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

let rec cnf: t -> cnf = function
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

let not_ f = Not f

let true_ = True

let false_ = False

let (&&&) x y = And (x, y)

let (|||) x y = Or (x, y)

let name t = t.name

let default t = t.default

let with_default t default =
  { t with default }

let create_elt ~doc ~default name =
  { name; default; doc }

let create ~doc ~default name =
  atom (create_elt ~doc ~default name)

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

let native_elt =
  create_elt
    ~doc:"native code compilation." ~default:true "native"

let native_dynlink_elt =
  create_elt
    ~doc:"native plugins for native code." ~default:true "native-dynlink"

let annot_elt =
  create_elt
    ~doc:"the generation of binary annotations." ~default:true "annot"

let debug_elt =
  create_elt
    ~doc:"the generation of debug symbols." ~default:true "debug"

let warn_error_elt =
  create_elt
    ~doc:"warning as errors." ~default:false "warn-error"

let test_elt =
  create_elt
    ~doc:"tests." ~default:false "test"

let public_doc_elt =
  create_elt
    ~doc:"the generation of public documentation." ~default:false "doc"

let js_elt =
  create_elt
    ~doc:"the generation of JavaScript build artefacts, using [js_of_ocaml]."
    ~default:false "js"

let full_doc_elt =
  create_elt
    ~doc:"the generation of the full documentation."
    ~default:false "full-doc"

let base = List.fold_left (fun set t -> Set.add t set) Set.empty [
    native_elt; native_dynlink_elt;
    debug_elt; annot_elt; warn_error_elt;
    test_elt; public_doc_elt; js_elt;
    full_doc_elt;
  ]

let native = atom native_elt
let native_dynlink = atom native_dynlink_elt
let annot = atom annot_elt
let warn_error = atom warn_error_elt
let debug = atom debug_elt
let test = atom test_elt
let public_doc = atom public_doc_elt
let js = atom js_elt
let full_doc = atom full_doc_elt
