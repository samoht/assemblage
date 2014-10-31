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

open As_action.Spec;;

(* Types *)

type includes = As_path.rel list As_conf.value
type name = As_path.rel As_conf.value

(* Global options *)

let debug = add_if_key As_conf.debug (atom "-g") @@ atoms []

(* Preprocess *)

let compile_src_ast src_kind ~src =
  let ctx = As_ctx.v [ `OCaml; `Pp; (src_kind :> As_ctx.elt) ] in
  let cond = As_conf.(value ocaml_build_ast) in
  let outfile = match src_kind with
  | `Ml -> path src ~ext:`Ml_pp
  | `Mli -> path src ~ext:`Ml_pp
  in
  let inputs = product src in
  let outputs = product outfile in
  let args =
    (* TODO ppx & pp args *)
    add (path_arg src) @@
    path_arg ~opt:"-o" outfile
  in
  let cmd = As_action.cmd As_conf.ocaml_dumpast args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

(* Compile *)

let compile_common =
  add_if_key As_conf.warn_error (atoms [ "-warn-error"; "+a" ]) @@
  add_if_key As_conf.ocaml_annot (atom "-bin-annot") @@
  debug

let compile_with_incs incs rest =
  add compile_common @@ add (paths_args ~opt:"-I" incs) @@ rest

let mli_compiler = (* don't fail if ocamlc is not available *)
  let open As_conf in
  let comp = pick_if (value ocaml_native) (value ocamlopt) (value ocamlc) in
  key ~public:false "ocamlc-mli" string comp

let compile_mli ~incs ~src =
  let ctx = As_ctx.v [`OCaml; `Compile; `Mli ] in
  let cond = As_conf.(value ocaml_native ||| value ocaml_byte) in
  let inputs = add (product src ~ext:`Mli_dep) @@ product src in
  let outputs =
    add_if_key As_conf.ocaml_annot (product src ~ext:`Cmti) @@
    product src ~ext:`Cmi
  in
  let args =
    compile_with_incs incs @@ add (atoms [ "-c"; "-intf" ]) @@ path_arg src
  in
  let cmd = As_action.cmd mli_compiler args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let compile_ml_byte ~has_mli ~incs ~src =
  let ctx = As_ctx.v [ `OCaml; `Compile; `Byte ] in
  let cond = As_conf.(value ocaml_byte) in
  let inputs =
    add_if has_mli (product src ~ext:`Cmi) @@
    add (product src ~ext:`Ml_dep) @@
    product src
  in
  let outputs =
    add_if has_mli (product src ~ext:`Cmi) @@
    add_if_key As_conf.ocaml_annot (product src ~ext:`Cmt) @@
    product src ~ext:`Cmo
  in
  let args =
    compile_with_incs incs @@ add (atoms [ "-c"; "-impl" ]) @@ path_arg src
  in
  let cmd = As_action.cmd As_conf.ocamlc args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let compile_ml_native ~has_mli ~incs ~src =
  let ctx = As_ctx.v [ `OCaml; `Compile; `Native ] in
  let cond = As_conf.(value ocaml_native) in
  let inputs =
    add_if has_mli (product src ~ext:`Cmi) @@
    add (product src ~ext:`Ml_dep) @@
    product src
  in
  let outputs =
    add_if (As_conf.neg has_mli) (product src ~ext:`Cmi) @@
    add_if_key As_conf.ocaml_annot (product src ~ext:`Cmt) @@
    product src ~ext:`Cmx
  in
  let args =
    add_if_key As_conf.profile (atom "-p") @@
    compile_with_incs incs @@
    add (atoms [ "-c"; "-impl" ]) @@ path_arg src
  in
  let cmd = As_action.cmd As_conf.ocamlopt args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

(* Archive *)

let archive_byte ~cmos ~name =
  let ctx = As_ctx.v [ `OCaml; `Archive; `Byte ] in
  let cond = As_conf.(value ocaml_byte) in
  let cma = path name ~ext:`Cma in
  let inputs = cmos in
  let outputs = product cma in
  let args =
    add (atoms ["-a"; "-o"]) @@ add (path_arg cma) @@ paths_args cmos
  in
  let cmd = As_action.cmd As_conf.ocamlc args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let archive_native ~cmx_s ~name =
  let ctx = As_ctx.v [ `OCaml; `Archive; `Native ] in
  let cond = As_conf.(value ocaml_native) in
  let cmxa = path name ~ext:`Cmxa in
  let inputs = cmx_s in
  let outputs = product cmxa in
  let args =
    add (atoms [ "-a"; "-o" ]) @@ add (path_arg cmxa) @@ paths_args cmx_s
  in
  let cmd = As_action.cmd As_conf.ocamlopt args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let archive_shared ~cmx_s ~name =
  let ctx = As_ctx.v [ `OCaml; `Archive; `Native; `Shared ] in
  let cond = As_conf.(value ocaml_native &&& value ocaml_native_dynlink) in
  let cmxs = path name ~ext:`Cmxs in
  let inputs = cmx_s in
  let outputs = product cmxs in
  let args =
    add (atoms [ "-shared"; "-o" ]) @@ add (path_arg cmxs) @@ paths_args cmx_s
  in
  let cmd = As_action.cmd As_conf.ocamlopt args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let c_compiler = (* don't fail if ocamlc is not available *)
  let open As_conf in
  let comp = pick_if (value ocaml_native) (value ocamlopt) (value ocamlc) in
  key ~public:false "ocamlc-c" string comp

let compile_c ~src =
  let ctx = As_ctx.v [ `OCaml; `C; `Compile ] in
  let cond = As_conf.(value ocaml_native ||| value ocaml_byte) in
  let inputs = product src in
  let outputs = product src ~ext:`O in
  let args = add (atom "-c") @@ path_arg src in
  let cmd = As_action.cmd c_compiler args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd

let archive_c ~objs ~name =
  let ctx = As_ctx.v [ `OCaml; `C; `Archive; `Shared ] in
  let cond = As_conf.(value ocaml_native ||| value ocaml_byte) in
  let inputs = objs in
  let outputs = add (product name ~ext:`A) @@ product name ~ext:`So in
  let args = add (atom "-o") @@ add (path_arg name) @@ paths_args objs in
  let cmd = As_action.cmd As_conf.ocamlmklib args in
  As_action.v ~cond ~ctx ~inputs ~outputs cmd
