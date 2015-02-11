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

let str = Printf.sprintf

let run_ocamlfind ocamlfind name =
  let open As_cmd.Infix in
  let err pkg = str "Could not lookup ocamlfind package %s" name in
  let args preds =
    let preds = String.concat "," @@ match name with
      | "threads.posix" -> "mt" :: "mt_posix" :: preds
      | "threads.vm" -> "mt" :: "mt_vm" :: preds
      | _ -> preds
    in
    [ "query"; "-predicates"; preds; "-r"; "-format"; "\"%d|%A|%O\"" ] @ [name]
  in
  begin
    As_cmd.read_lines ocamlfind (args ["byte"]) >>= fun byte ->
    As_cmd.read_lines ocamlfind (args ["native"]) >>= fun native ->
    As_cmd.read_lines ocamlfind (args ["syntax"; "preprocessor"]) >>= fun pp ->
    As_cmd.ret (byte, native, pp)
  end
  |> As_cmd.reword_error (err name)
  |> As_cmd.on_error ~use:([],[],[])

type pkg =
  { byte_incs : string list;
    byte_objs : string list; (* full path *)
    byte_link : string list;
    native_incs : string list;
    native_objs : string list;
    native_link : string list;
    pp_incs : string list;
    pp_objs : string list; }

let parse_lines (byte, native, pp) =
  let add_line (i, o, f as acc) l =
    match As_string.split ~sep:"|" l with
    | [dir; objs; flags] ->
        let objs = As_string.split ~sep:" " objs in
        let objs = List.filter ((<>)"") objs in
        let objs = List.map (fun obj -> str "%s/%s" dir obj) objs in
        let flags = As_string.split ~sep:" " flags in
        let flags = List.filter ((<>)"") flags in
        dir :: "-I" :: i, List.rev_append objs o, List.rev_append flags f
    | _ ->
        As_log.err "ocamlfind lookup could not parse line (%s)" l;
        acc
  in
  let parse lines =
    let i, o, f = List.fold_left add_line ([], [], []) lines in
    List.rev (As_string.uniquify i),
    List.rev (As_string.uniquify o),
    List.rev f
  in
  let byte_incs, byte_objs, byte_link = parse byte in
  let native_incs, native_objs, native_link = parse native in
  let pp_incs, pp_objs, _ = parse pp in
  { byte_incs; byte_objs; byte_link;
    native_incs; native_objs; native_link;
    pp_incs; pp_objs; }

let pkg_lookups ocamlfind name =
  let p = run_ocamlfind ocamlfind name |> parse_lines in
  [ As_ctx.v [`OCaml; `Pp], p.pp_incs @ p.pp_objs;
    As_ctx.v [`OCaml; `Compile; `Target `Byte], p.byte_incs;
    As_ctx.v [`OCaml; `Compile; `Target `Native], p.native_incs;
    As_ctx.v [`OCaml; `Compile; `Target `Byte; `Src `Mli], p.byte_incs;
    As_ctx.v [`OCaml; `Compile; `Target `Native; `Src `Mli], p.native_incs;
    As_ctx.v [`OCaml; `Link; `Target `Byte], p.byte_objs @ p.byte_link;
    As_ctx.v [`OCaml; `Link; `Target `Native], p.native_objs @ p.native_link; ]

let lookup name =
  let lookups = As_conf.(const pkg_lookups $ (value ocamlfind) $ const name) in
  let lookup lookups ctx =
      let add acc (pkg_ctx, args) =
        if not (As_ctx.matches pkg_ctx ctx) then acc else
        List.rev_append (List.rev args) acc
      in
      List.fold_left add [] lookups
  in
  As_conf.(const lookup $ lookups)
