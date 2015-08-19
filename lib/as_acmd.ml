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


(* Action commands *)

open Bos

type cmd = string As_conf.key option * string

let cmd k = As_conf.(const (fun k v -> Some k, v) $ const k $ value k)
let static n = None, n

type t =
  { cmd : cmd;                                                  (* command. *)
    args : string list;                               (* command arguments. *)
    stdin : path option;                     (* stdin redirection (if any). *)
    stdout : path option;                   (* stdout redirection (if any). *)
    stderr : path option; }                 (* stderr redirection (if any). *)

let v ?stdin ?stdout ?stderr cmd args =
  { cmd; args; stdin; stdout; stderr }

let cmd_key c = fst c.cmd
let cmd_name c = snd c.cmd
let args c = c.args
let stdin c = c.stdin
let stdout c = c.stdout
let stderr c = c.stderr

let pp ppf c =
  let pp_redir fdname ppf = function
  | None -> ()
  | Some p -> Fmt.pf ppf "%s %s" fdname (Path.to_string p)
  in
  Fmt.pf ppf "@[%a%s @[%a%a%a%a@]@]"
    Fmt.(option (fun ppf k -> Fmt.pf ppf "%s:" (As_conf.Key.name k)))
    (cmd_key c)
    (cmd_name c)
    Fmt.(list ~sep:sp string) c.args
    (pp_redir "<") c.stdin
    (pp_redir "1>") c.stdout
    (pp_redir "2>") c.stderr

let ctx context c =
  let elt = match fst c.cmd with
  | None -> `Cmd_static (snd c.cmd)
  | Some k -> `Cmd k
  in
  As_ctx.add elt context

let args_with_ctx conf context args c =
  let injected = As_args.for_ctx conf (ctx context c) args in
  List.rev_append (List.rev injected) c.args

module Args = struct
  let add a al = a :: al
  let adds al al' = List.rev_append (List.rev al) al'
  let add_if c a al = if c then add a al else al
  let adds_if c al al' = if c then adds al al' else al'
  let fadd_if c f v al = if c then add (f v) al else al
  let fadds_if c f v al = if c then adds (f v) al else al
  let path_arg ?opt p al = match opt with
  | None -> Path.to_string p :: al
  | Some opt -> opt :: Path.to_string p :: al

  let path_args ?opt ps al = match opt with
  | None -> List.rev_append (List.rev_map Path.to_string ps) al
  | Some opt ->
      let add acc p = (Path.to_string p) :: opt :: acc in
      List.rev_append (List.fold_left add [] ps) al

  let path p ~ext = Path.set_ext p ext
end

(** {1 Portable system utility invocations} *)

open Args

let dev_null =
  let dev_null os = match os with
  | "Win32" -> Path.v "NUL"
  | _ -> Path.(root / "dev" / "null")
  in
  As_conf.(const dev_null $ value host_os)

let cd =
  let make_cmd cd = fun dir -> v cd (path_arg dir @@ []) in
  As_conf.(const make_cmd $ cmd As_conf.cd)

let ln =
  let make_cmd os ln = match os with
  | "Win32" ->
      (* TODO *)
      Log.warn "Symbolic@ links@ unsupported@ copying@ instead.";
      fun src dst -> v ln (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst ->
        let args = adds ["-s"; "-f"] @@ path_arg src @@ path_arg dst @@ [] in
        v ln args
  in
  As_conf.(const make_cmd $ (value host_os) $ (cmd As_conf.ln ))

let ln_rel =
  let relativize ~root src = (* src as seen from dst, as short as possible *)
    match Path.relativize ~root src with
    | None -> assert false (* TODO *)
    | Some p -> p
  in
  let make_cmd ln src dst = ln (relativize ~root:dst src) dst in
  let ln_cmd = ln in
  As_conf.(const make_cmd $ ln_cmd)

let cp =
  let make_cmd os cp = match os with
  | "Win32" ->
      fun src dst -> v cp (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst -> v cp (path_arg src @@ path_arg dst @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.cp)

let mv =
  let make_cmd os mv = match os with
  | "Win32" ->
      fun src dst -> v mv (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst -> v mv (path_arg src @@ path_arg dst @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.mv)

let rm_files =
  let make_cmd os rm = match os with
  | "Win32" ->
      fun ?(f = false) paths ->
        v rm (add_if f "/F" @@ add "/Q" @@ path_args paths @@ [])
  | _ ->
      fun ?(f = false) paths ->
        v rm (add_if f "-f" @@ path_args paths @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.rm)

let rm_dirs =
  let make_cmd os rmdir = match os with
  | "Win32" ->
      fun ?(f = false) ?(r = false) paths ->
        v rmdir (add_if f "/F" @@ add_if r "/S" @@ add "/Q" @@
               path_args paths @@ [])
  | _ ->
      fun ?(f = false) ?(r = false) paths ->
        v rmdir (add_if f "-f" @@ add_if r "-r" @@ path_args paths @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.rmdir)

let mkdir =
  let make_cmd os mkdir = match os with
  | "Win32" -> fun dir -> v mkdir (path_arg dir @@ [])
  | _ -> fun dir -> v mkdir (add "-p" @@ path_arg dir @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ cmd As_conf.mkdir)

let stamp =
  let make_cmds cmd file contents = v cmd [contents] ~stdout:file in
  As_conf.(const make_cmds $ cmd As_conf.echo)
