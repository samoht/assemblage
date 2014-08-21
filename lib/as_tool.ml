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

let (|>) x f = f x

let log_project env p =
  let post =
    if not env.As_env.utf8_msgs then "" else
    " \xF0\x9F\x8D\xB7" (* UTF-8 <U+1F377, U+0020, U+0020> *)
  in
  Printf.printf "%s %s %s%s\n" (As_shell.color `Black "==>")
    (As_shell.color `Bold (As_project.name p)) (As_project.version p) post

let check t =
  let components = As_project.components t in
  let check_dumpast () = (* check we have dumpast if there are pp. *)
    let pkg_pp = As_component.(filter_map pkg_ocaml_pp) components in
    let lib_pp = As_component.(filter_map lib_ocaml_pp) components in
    if (pkg_pp <> [] || lib_pp <> []) && not (As_shell.has_cmd "ocaml-dumpast")
    then
      As_shell.warn
        "ocaml-dumpast is needed to setup a project using camlp4 syntax \
         extensions."
  in
  let check_pkgs () = (* check that all required packages installed. *)
    let missing =
      let missing acc pkg =
        let opt = As_component.Pkg.opt pkg in
        let name = As_component.name (`Pkg pkg) in
        if not opt && not (As_shell.try_exec "ocamlfind query %s" name)
        then name :: acc
        else acc
      in
      List.fold_left missing [] (As_component.(filter_map pkg) components)
    in
    match missing with
    | [] -> ()
    | pkg :: [] ->
        As_shell.warn "The required ocamlfind package %s is not installed." pkg
    | pkg :: pkgs ->
        As_shell.warn "The required ocamlfind packages %s and %s are not \
                       installed."
          (String.concat " " pkgs) pkg
  in
  check_pkgs ();
  check_dumpast ();
  ()

let setup p env build_env dumpast `Make =
  let features = As_build_env.features build_env in
  let flags = As_build_env.flags build_env in
  let makefile = "Makefile" in
  let merlin = ".merlin" in
  let build_dir = As_build_env.build_dir build_env in
  let file_arrow = As_shell.color `Green "==>" in
  check p;
  log_project env p;
  Printf.printf "%s write %s\n" file_arrow makefile;
  As_makefile.write_file makefile
    (As_project_makefile.of_project p
       ~features ~flags ~makefile ~merlin ~dumpast);
  As_ocamlfind.META.(write (of_project p));
  As_opam.Install.(write (of_project ~build_dir p));
  Printf.printf "%s write %s\n" file_arrow merlin;
(*  As_merlin.(write_file merlin (of_project ~build_dir p)); *)
  `Ok ()

let describe p env build_env =
  let open Printf in
  let print_deps x =
    let bold_name pkg = As_shell.color `Bold (As_component.Pkg.name pkg) in
    let pkgs = As_component.(filter_map pkg x) in
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
          As_shell.color `Cyan (As_component.name (`Unit u) ^ ext)
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
  let print cs =
    let aux c =
      let open As_component in
      printf "└─┬─ %s\n%s"
        (As_shell.color `Magenta (id c)) (print_deps (deps c));
        print_units (filter_map unit (contents c)) in
    List.iter aux cs in
  let components =
    As_project.components p
    |> List.filter (function `Lib _ | `Bin _ -> true | _ -> false)
  in
  log_project env p;
  print components;
  `Ok ()
