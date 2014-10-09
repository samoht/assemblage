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

let log_project env version p =
  let post =
    if not env.Assemblage_env.utf8_msgs then "" else
    " \xF0\x9F\x8D\xB7" (* UTF-8 <U+1F377, U+0020, U+0020> *)
  in
  Log.show "%a %a %s%s"
    Fmt.pp_rarrow () (Fmt.pp_styled_str `Bold) (Project.name p) version post

let check t =
  let parts = Project.parts t in
  let check_dumpast () = (* check we have dumpast if there are pp. *)
    let pkg_pp = [] in
(*
      TODO create a dumb env and query Part.args and get (`Pp `Byte)
      As_args.get (`Pp `byte) (Part.(keep_map Pkg.ocaml) parts
      in
*)
    let lib_pp = Part.(keep_map Lib.ocaml_pp) parts in
    if (pkg_pp <> [] || lib_pp <> []) && not (Asd_shell.has_cmd "ocaml-dumpast")
    then
      Log.warn "%a" Fmt.pp_text
        "ocaml-dumpast is needed to setup a project using camlp4 syntax \
         extensions."
  in
  let check_ocaml_pkgs () = (* check that all required packages installed. *)
    let missing =
      let missing acc pkg =
        let name = Part.name pkg in
        if not (Asd_shell.try_exec "ocamlfind query %s" name)
        then name :: acc
        else acc
      in
      let pkgs = Part.(keep_map Pkg.ocaml parts)
      in
      List.fold_left missing [] pkgs
    in
    match missing with
    | [] -> ()
    | pkg :: [] ->
        Log.warn
          "The@ required@ ocamlfind@ package@ %s@ is@ not@ installed." pkg
    | pkg :: pkgs ->
        Log.warn
          "The@ required@ ocamlfind@ packages@ %a@ and@ %s are@ not@ installed."
          Fmt.(pp_list ~pp_sep:(fun ppf () -> pp ppf "@, ") pp_str) pkgs pkg
  in
  check_ocaml_pkgs ();
  check_dumpast ();
  ()

let setup ~version p env build_env dumpast `Make ~merlin =
  let atomic_conds = Asd_setup_env.atomic_conds build_env in
  let args = Asd_setup_env.args build_env in
  let makefile = "Makefile" in
  let build_dir = Asd_setup_env.build_dir build_env in
  let pp_file_arrow = Fmt.(pp_styled `Green pp_rarrow) in
  let merlin_file = ".merlin" in
  let clean_files = if merlin then [merlin_file] else [] in
  check p;
  log_project env version p;
  Log.show "%a write %s" pp_file_arrow () makefile;
  Asd_makefile.write_file makefile
    (Asd_project_makefile.of_project ~version p ~atomic_conds ~args ~makefile
       ~dumpast
       ~clean_files);
  Asd_ocamlfind.META.(write (of_project ~version p));
  Asd_opam.Install.(write (of_project ~build_dir p));
  if merlin then begin
    Log.show "%a write %s" pp_file_arrow () merlin_file;
    (Asd_merlin.(write_file merlin_file (of_project ~build_dir p)));
  end;
  `Ok ()
