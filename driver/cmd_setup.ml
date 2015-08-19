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

open Assemblage
open Assemblage.Private
open Assemblage_tools

let str = Printf.sprintf

let write file s =
  let pp_arrow = Fmt.(styled `Green @@ verbatim "==>") in
  Log.show "%a write %s" pp_arrow () (Path.to_string file);
  Log.on_error_msg ~use:() @@
  OS.File.write file s;
  ()

let write_meta p file = write file Meta.(to_string @@ of_project p)
let write_merlin p file = write file Merlin.(to_string @@ of_project p)
let write_install p meta file =
  let add = [ `Lib (Opam.Install.move meta) ] in
  write file Opam.Install.(to_string @@ of_project ~add p)

let write_makefile p ~setup_files file =
  let mk = Builder_makefile.of_project ~setup_files p in
  write file (Makefile.to_string mk)

let setup `Make ~merlin p =
  let add_if c v acc = if c then v :: acc else acc in
  let install = Path.v (str "%s.install" @@ Project.name p) in
  let dotmerlin = Path.v ".merlin" in
  let makefile = Path.v "Makefile" in
  let meta = Path.(Project.eval_key p Conf.build_dir / "META") in
  let setup_files =
    add_if merlin dotmerlin @@ install :: meta :: makefile :: []
  in
  Log.show "%a@." Project.pp_signature p;
  write_meta p meta;
  write_install p meta install;
  write_makefile p ~setup_files makefile;
  if merlin then write_merlin p dotmerlin;
  `Ok ()

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "setup an assemblage project" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,setup) command generates a build system to build the
          parts defined in an assemble.ml file. An initial configuration
          for the build system can be specified on the command line by
          specifying configuration keys using the flags described below."; ]
  in
  let see_also = ["build"; "describe"] in
  let merlin_opt =
    let doc = "Generate a .merlin file." in
    Arg.(value & opt bool true & info ["merlin"] ~doc ~docv:"BOOL")
  in
  let setup make merlin = setup make ~merlin in
  let setup = Term.(pure setup $ pure `Make $ merlin_opt) in
  Cmd_base.cmd_with_project "setup" setup ~doc ~man ~see_also
