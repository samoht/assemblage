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

let str = Printf.sprintf

let describe p =
  Log.show "%a" Project.pp_signature p;
  Log.show "%a" Conf.pp (Project.conf p);
  `Ok ()

(*
  let version = "FIXME" in
  let open Printf in
  let print_deps x = (* TODO *) ()
    let bold_name pkg = As_shell.color `Bold (As_part.name pkg) in
    let pkgs = As_part.(keep_kind `Pkg x) in
    match String.concat " " (List.map bold_name pkgs) with
    | "" -> ""
    | pkgs -> sprintf "  ├─── [%s]\n" pkgs
  in
  let print_modules last modules =
    let aux i n m =
      printf "  %s %s\n"
        (if last && i = n then "└───" else "├───") (As_shell.color `Blue m) in
    let n = List.length modules - 1 in
    List.iteri (fun i m -> aux i n m) modules in
  let print_units units =
    let aux i n u =
        let mk f ext =
          if not (f u) then "" else
          As_shell.color `Cyan (As_part.name u ^ ext)
        in
        let ml = mk As_component.Unit.(has `Ml) ".ml" in
        let mli = mk As_component.Unit.(has `Mli) ".mli" in
        let modules =
          if As_component.Unit.generated u then ["--generated--"]
          else
          let build_dir = As_build_env.build_dir build_env in
          As_OCaml.modules ~build_dir u in
        printf "  %s %-25s%-25s\n"
          (if modules = [] && i = n then "└─" else "├─") ml mli;
        print_modules (i=n) modules
    in
    let n = List.length units - 1 in
    List.iteri (fun i u -> aux i n u) units in
  in
  let print cs = ()
    (* TODO *)

    let aux c =
      let open As_component in
      printf "└─┬─ %s\n%s"
        (As_shell.color `Magenta (id c)) (print_deps (deps c));
        print_units (filter_map unit (contents c)) in
    List.iter aux cs in

  in
  let parts = Part.keep_kinds [`Lib; `Bin] (Project.parts p) in
  log_project env version p;
  print parts;
*)


(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "describe an OCaml project" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,describe) command outputs various descriptions of a
          configured project." ]
  in
  let describe = Term.(pure describe) in
  Cmd_base.cmd_with_project "describe" describe ~doc ~man ~see_also:["setup"]
