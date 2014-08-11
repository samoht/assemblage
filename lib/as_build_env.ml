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

open Cmdliner

type t =
  { features : (As_features.atom * bool) list;
    flags : As_flags.t;
    build_dir : string; }

let create
    ?(features = [])
    ?(flags = As_flags.empty)
    ?(build_dir = "_build")
    () =
  { features; flags; build_dir; }

let build_dir t = t.build_dir
let features t = t.features
let flags t = t.flags
let default =
  { features = [];
    flags = As_flags.empty;
    build_dir = "_build"; }

let enable t flags =
  List.for_all (fun f ->
      try List.assoc f t.features
      with Not_found -> false
    ) flags

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

let term features : t Cmdliner.Term.t =
  let features = As_features.Set.elements features in
  let features =
    let term_of_list list =
      let add acc h = Term.(pure (fun f t -> f :: t) $ h $ acc) in
      List.fold_left add (Term.pure []) list
    in
    term_of_list (List.map As_features.parse features)
  in
  let list = function None -> [] | Some l -> [l] in
  let create features comp link pp build_dir =
    let link = list link in
    let comp = list comp in
    let pp = list pp in
    let flags =
      let open As_flags in
      v (`Compile `Byte) comp @@@
      v (`Compile `Native) comp @@@
      v (`Link `Byte) link @@@
      v (`Link `Native) link @@@
      v (`Pp `Byte) pp @@@
      v (`Pp `Native) pp
    in
    create ~features ~flags ~build_dir ()
  in
  Term.(pure create $ features $ comp_opt $ link_opt $ pp_opt $ build_dir_opt)
