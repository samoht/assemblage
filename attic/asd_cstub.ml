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



let cstubs ?available ?(deps = []) ?(headers = []) ?(cflags = []) ?(clibs = [])
    name ~dir
  =
  let name_bindings = name ^ "_bindings" in
  let name_stubs = name ^ "_stubs" in

  (* 1. compile the bindings. *)
  let deps = `Pkg As_part.Pkg.ctypes_stub :: deps in
  let bindings = unit name_bindings (`Path path) ~deps in

  (* 2. compile the generator of <name>_stubs.{ml,c} and <name>.ml *)
  let generator =
    let name_generator = name ^ "_generator" in
    let ctypes_gen =
      other (name ^ "-generator") [
        As_action.rule
          ~phase:`Prepare
          ~targets:[`Self `Ml]
          ~prereqs:[]
          (fun _t r _f ->
             let dir = As_part.build_dir bindings r in
             let ml_stubs = dir / name_stubs ^ ".ml" in
             let c_stubs  = dir / name_stubs ^ ".c" in
             let library  = dir / name ^ ".ml" in
             let headers = match headers with
             | [] -> ""
             | hs -> sprintf "--headers %s " (String.concat "," hs) in
             As_action.create ~dir
               "ctypes-gen %s--ml-stubs %s --c-stubs %s --library %s %s"
               headers ml_stubs c_stubs library name)
      ] in
    let unit = unit name_generator ctypes_gen in
    bin name_generator (`Units [unit])
  in

  (* 3. compile the generated stubs *)
  let run_generator =
    other (name ^ "-generator.run") [
      As_action.rule
        ~phase:`Prepare
        ~targets:[`Self `Ml; `Self `C]
        ~prereqs:[`N (generator, `Byte)]
        (fun t r _f ->
           let dir = As_part.build_dir t r in
           As_action.create ~dir "./%s.byte" (As_part.name t))
    ] in
  let ml_stubs = unit name_stubs run_generator ~deps:[bindings] in
  let c_stubs = c name_stubs run_generator in
  let main = unit name run_generator ~deps:[ml_stubs; c_stubs] in

  (* 4. compile the main library *)
  let flags =
    let link_flags = cflags @ List.map (sprintf "-l%s") clibs in
    As_flags.(cclib link_flags @@@ stub name_stubs) in
  (* FIXME: which action ? *)
  lib name ~flags ?available (`Units [bindings; ml_stubs; c_stubs; main])
