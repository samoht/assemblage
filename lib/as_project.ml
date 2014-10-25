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

(* Project *)

type t =
  { name : string;
    cond : bool As_conf.value;
    args : As_args.t;
    parts : As_part.kind As_part.t list; }

let create ?(cond = As_conf.true_) ?(args = As_args.empty) name parts =
  { name; cond; args;
    parts = (parts :> As_part.kind As_part.t list); }

let name t = t.name
let cond t = t.cond
let args t = t.args
let parts t = t.parts
let conf t = (* TODO *)
  let ( + ) = As_conf.add in
  let open As_conf in
  let o = key ~public:true ~docs:"BLA" "ocamlc" string (const "u") in
  empty + o + debug + profile + test + doc + jobs +
  root_dir + build_dir + product_dir
  + ocaml_native_tools + ocaml_byte + ocaml_native +
  ocaml_native_dynlink + ocaml_js + ocaml_annot + warn_error +
  ocaml_build_ast + ocaml_dumpast +
  ocamlc + ocamlopt + js_of_ocaml + ocamldep + ocamlmklib +
  ocamldoc + ocamllex + ocamlyacc + ocaml + ocamlrun + ocamldebug + ocamlprof +
  ocamlfind + ocaml_version + echo + ln + cp + mkdir + cat + make + cc +
  pkg_config + uname + host_os + host_arch + target_os + target_arch + opam + opam_installer + opam_admin + project_version

let projects = ref []
let assemble p = projects := p :: !projects
let list () = List.rev !projects
