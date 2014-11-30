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

(* The assemblage command line tool. *)

open Cmdliner
open Assemblage
open Assemblage.Private

let main () =
  let cmd = if Array.length Sys.argv < 2 then None else Some Sys.argv.(1) in
  match cmd with
  | Some ("b" | "bu" | "bui" | "buil" | "build") -> Cmd_build.main ()
  | _ ->
      let cmds = [ Cmd_setup.cmd; Cmd_describe.cmd; Cmd_product.cmd ] in
      let cmds = Cmd_base.terms cmds in
      match Term.eval_choice (List.hd cmds) (List.tl cmds) with
      | `Error _ -> exit 1
      | `Ok () | `Version | `Help ->
          if Log.err_count () <> 0 then exit 1 else exit 0

let () = main ()
