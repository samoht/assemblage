(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Daniel C. Buenzli
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

open Assemblage
open Cmdliner


type t =
  { atomic_conds : (Cond.atom * bool) list;
    args : args;
    build_dir : string; }

let create
    ?(atomic_conds = [])
    ?(args = Args.empty)
    ?(build_dir = "_build")
    () =
  { atomic_conds; args; build_dir; }

let build_dir t = t.build_dir
let atomic_conds t = t.atomic_conds
let args t = t.args
let default =
  { atomic_conds = [];
    args = Args.empty;
    build_dir = "_build"; }

(* Build environment base command line options *)

let comp_opt =
  let doc = "Additional options given to the native and bytecode compilers." in
  Arg.(value & opt (some string) None & info ["comp"] ~doc ~docv:"OPTIONS")

let link_opt =
  let doc = "Additional options given to the native and bytecode linkers." in
  Arg.(value & opt (some string) None & info ["link"] ~doc ~docv:"OPTIONS")

let pp_opt =
  let doc = "Additional options given to the pre-processor." in
  Arg.(value & opt (some string) None & info ["pp"] ~doc ~docv:"OPTIONS")

let build_dir_opt =
  let doc = "Name of the directory where built artifacts are created." in
  Arg.(value & opt string "_build" & info ["build-dir"] ~doc ~docv:"DIR")

let parse_atom a =
  let docv = "BOOL" in
  let doc = Cond.atom_doc a in
  let default = Cond.default a in
  let name = Cond.name a in
  let opt = Arg.(value & opt bool default & info [name] ~doc ~docv) in
  Term.(pure (fun value -> a, value) $ opt)


let term atoms : t Cmdliner.Term.t =
  let atoms = Assemblage.Cond.Set.elements atoms in
  let atoms =
    let term_of_list list =
      let add acc h = Term.(pure (fun f t -> f :: t) $ h $ acc) in
      List.fold_left add (Term.pure []) list
    in
    term_of_list (List.map parse_atom atoms)
  in
  let list = function None -> [] | Some l -> [l] in
  let create atomic_conds comp link pp build_dir =
    let link = list link in
    let comp = list comp in
    let pp = list pp in
    let args =
      let open Assemblage.Args in
      create (`Compile `Byte) comp @@@
      create (`Compile `Native) comp @@@
      create (`Link `Byte) link @@@
      create (`Link `Native) link @@@
      create (`Pp `Byte) pp @@@
      create (`Pp `Native) pp
    in
    create ~atomic_conds ~args ~build_dir ()
  in
  Term.(pure create $ atoms $ comp_opt $ link_opt $ pp_opt $ build_dir_opt)
