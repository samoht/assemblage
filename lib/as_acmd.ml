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

type bin = string As_conf.key * string
let bin b = As_conf.(const (fun k v -> k, v) $ const b $ (value b))

type t =
  { bin_key : string As_conf.key;      (* Remember the key for the context. *)
    bin : string;                                      (* value of bin_key. *)
    args : string list;                               (* command arguments. *)
    stdin : As_path.t option;                (* stdin redirection (if any). *)
    stdout : As_path.t option;              (* stdout redirection (if any). *)
    stderr : As_path.t option; }            (* stderr redirection (if any). *)

let v ?stdin ?stdout ?stderr (bin_key, bin) args =
  { bin_key; bin; args; stdin; stdout; stderr }

let bin_key c = c.bin_key
let bin_name c = c.bin
let args c = c.args
let stdin c = c.stdin
let stdout c = c.stdout
let stderr c = c.stderr

let pp ppf c =
  let pp_redir fdname ppf = function
  | None -> ()
  | Some p -> As_fmt.pp ppf "%s %s" fdname (As_path.to_string p)
  in
  As_fmt.pp ppf "@[%s: @[%s @[%a%a%a%a@]@]@]"
    (As_conf.Key.name c.bin_key) c.bin
    As_fmt.(pp_list ~pp_sep:pp_sp pp_str) c.args
    (pp_redir "<") c.stdin
    (pp_redir "1>") c.stdout
    (pp_redir "2>") c.stderr

let ctx context c = As_ctx.add (`Cmd c.bin_key) context
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
  | None -> As_path.to_string p :: al
  | Some opt -> opt :: As_path.to_string p :: al

  let path_args ?opt ps al = match opt with
  | None -> List.rev_append (List.rev_map As_path.to_string ps) al
  | Some opt ->
      let add acc p = opt :: (As_path.to_string p) :: acc in
      List.rev_append (List.fold_left add [] ps) al

  let path p ~ext = As_path.change_ext p ext
end

(** {1 Portable system utility invocations} *)

open Args

let dev_null =
  let dev_null os = match os with
  | "Win32" -> As_path.file "NUL"
  | _ -> As_path.(root / "dev" / "null")
  in
  As_conf.(const dev_null $ value host_os)

let ln =
  let make_cmd os bin = match os with
  | "Win32" ->
      As_log.warn "Symbolic@ links@ unsupported@ copying@ instead.";
      fun src dst -> v bin (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst ->
        let args = adds ["-s"; "-f"] @@ path_arg src @@ path_arg dst @@ [] in
        v bin args
  in
  As_conf.(const make_cmd $ (value host_os) $ (bin As_conf.ln ))

let ln_rel =
  (* FIXME here we really mean link src to dst when seen from
       the empty relative directory. We are using `..` but As_path.t are
       not supposed to have such segments. Really need to sort out
       paths. *)
  let see src ~from:dst = (* src as seen from dst, as short as possible *)
    let parent = Filename.parent_dir_name in
    let rec loop src dst = match As_path.Rel.dirname dst with
    | d when As_path.Rel.is_empty d -> src
    | d -> loop (As_path.Rel.(base parent // src)) d
    in
    match As_path.to_rel src, As_path.to_rel dst with
    | Some src, Some dst ->
        let pre = As_path.Rel.find_prefix src dst in
        let rem p = match As_path.Rel.rem_prefix pre p with
        | Some p -> p | None -> assert false
        in
        As_path.(of_rel (loop (rem src) (rem dst)))
    | _ -> src
  in
  let make_cmd ln src dst = ln (see src ~from:dst) dst in
  let ln_cmd = ln in
  As_conf.(const make_cmd $ ln_cmd)

let cp =
  let make_cmd os bin = match os with
  | "Win32" ->
      fun src dst -> v bin (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst -> v bin (path_arg src @@ path_arg dst @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ bin As_conf.cp)

let mv =
  let make_cmd os bin = match os with
  | "Win32" ->
      fun src dst -> v bin (add "/Y" @@ path_arg src @@ path_arg dst @@ [])
  | _ ->
      fun src dst -> v bin (path_arg src @@ path_arg dst @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ bin As_conf.mv)

let rm_files =
  let make_cmd os bin = match os with
  | "Win32" ->
      fun ?(f = false) paths ->
        v bin (add_if f "/F" @@ add "/Q" @@ path_args paths @@ [])
  | _ ->
      fun ?(f = false) paths ->
        v bin (add_if f "-f" @@ path_args paths @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ bin As_conf.rm)

let rm_dirs =
  let make_cmd os bin = match os with
  | "Win32" ->
      fun ?(f = false) ?(r = false) paths ->
        v bin (add_if f "/F" @@ add_if r "/S" @@ add "/Q" @@
               path_args paths @@ [])
  | _ ->
      fun ?(f = false) ?(r = false) paths ->
        v bin (add_if f "-f" @@ add_if r "-r" @@ path_args paths @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ bin As_conf.rmdir)

let mkdir =
  let make_cmd os bin = match os with
  | "Win32" -> fun dir -> v bin (path_arg dir @@ [])
  | _ -> fun dir -> v bin (add "-p" @@ path_arg dir @@ [])
  in
  As_conf.(const make_cmd $ value host_os $ bin As_conf.mkdir)

let stamp =
  let make_cmds bin file contents = v bin [contents] ~stdout:file in
  As_conf.(const make_cmds $ bin As_conf.echo)
