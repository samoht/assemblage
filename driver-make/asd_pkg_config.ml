(*
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

let str = Printf.sprintf
type syntax = [ `Shell | `Makefile ]
type mode = [ `Static | `Dynamic of [`Shell | `Makefile] ]

let query_args ?wrap ~opts pkgs =
  (* FIXME support wrap *)
  [ "pkg-config" ] @ opts @ pkgs

let query_static =
  let cache = Hashtbl.create 124 in
  let run cmd = try Hashtbl.find cache cmd with
  | Not_found ->
      let r = Asd_shell.exec_output "%s" cmd in
      Hashtbl.add cache cmd r;
      r
  in
  fun ?wrap ~opts pkgs ->
    let cmd = query_args ?wrap ~opts pkgs in
    run (String.concat " " cmd)

let query_makefile ?wrap ~opts pkgs =
  [ str "$(shell %s)" (String.concat " " (query_args ?wrap ~opts pkgs)) ]

let query ~mode = match mode with
| `Static -> query_static
| `Dynamic `Shell -> query_args
| `Dynamic `Makefile -> query_makefile

let cflags ?wrap ~mode pkgs = query ~mode ?wrap ~opts:["--cflags"] pkgs
let cflags_I ?wrap ~mode pkgs = query ~mode ?wrap ~opts:["--cflags-only-I"] pkgs
let cflags_other ?wrap ~mode pkgs =
  query ~mode ?wrap ~opts:["--cflags-only-other"] pkgs

let libs ?wrap ~mode pkgs = query ~mode ?wrap ~opts:["--libs"] pkgs
let libs_l ?wrap ~mode pkgs = query ~mode ?wrap ~opts:["-libs-only-l"] pkgs
let libs_L ?wrap ~mode pkgs = query ~mode ?wrap ~opts:["-libs-only-L"] pkgs
let libs_other ?wrap ~mode pkgs =
  query ~mode ?wrap ~opts:["-libs-only-other"] pkgs

let pkgs_args ~mode = function
| [] -> Args.empty
| pkgs ->
    let ocaml_clink_flags =
      (libs_l ~wrap:"-cclib" ~mode pkgs) @
      (libs_L ~wrap:"-ccopt" ~mode pkgs) @
      (libs_other ~wrap:"-ccopt" ~mode pkgs)
    in
    let ocamlmklib_flags =
      (libs_l ~mode pkgs) @
      (libs_L ~mode pkgs) @
      (libs_other ~wrap:"-ldopt" ~mode pkgs)
    in
    Args.concat [
      Args.create (`Pp `C) (cflags ~mode pkgs);
      Args.create (`Compile `C) (cflags ~wrap:"-ccopt" ~mode pkgs);
      Args.create (`Link `C) (ocaml_clink_flags);
      Args.create (`Archive `C) (ocamlmklib_flags);
      Args.create (`Archive `C_shared) (ocamlmklib_flags);
      Args.create (`Link `Byte) (ocaml_clink_flags);
      Args.create (`Link `Native) (ocaml_clink_flags);
      Args.create (`Archive `Byte) (ocaml_clink_flags);
      Args.create (`Archive `Native) (ocaml_clink_flags);
      Args.create (`Archive `Shared) (ocaml_clink_flags); ]

let available () = Asd_shell.has_cmd "pkg-config"
