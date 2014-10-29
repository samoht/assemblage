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
let (|>) x f = f x

let err_coerce k exp = str "part has kind %s not %s" k exp

(* Types *)

type kind =
  [ `Base | `Unit | `Lib | `Bin | `Pkg | `Run | `Doc | `Dir | `Silo | `Custom ]

type part_kind = kind

let kind_to_string = function
| `Base -> "base" | `Unit -> "unit" | `Lib -> "lib" | `Bin -> "bin"
| `Pkg -> "pkg" | `Run -> "run" | `Doc -> "doc" | `Silo -> "silo"
| `Dir -> "dir" | `Custom -> "custom"

type base_meta = unit

type unit_ocaml_interface = [ `Normal | `Opaque | `Hidden ]
type unit_ocaml_unit = [ `Mli | `Ml | `Both ]
type unit_c_unit = [ `C | `H | `Both ]
type unit_kind =
  [ `OCaml of unit_ocaml_unit * unit_ocaml_interface
  | `C of unit_c_unit
  | `Js ]
type unit_meta =
  { unit_kind : unit_kind;
    unit_src_dir : As_path.rel }

type lib_kind = [ `OCaml | `OCaml_pp | `C ]
type lib_meta =
  { lib_kind : lib_kind;
    lib_byte : bool;
    lib_native : bool;
    lib_native_dynlink : bool; }

type bin_kind = [ `OCaml | `OCaml_toplevel | `C ]
type bin_meta =
  { bin_kind : bin_kind; bin_native : bool; bin_byte : bool; bin_js : bool }

type pkg_kind = [ `OCaml | `C ]
type pkg_meta = { pkg_kind : pkg_kind; }

type run_meta = { run_dir : As_path.t }

type doc_kind = [ `OCamldoc ]
type doc_meta = { doc_kind : doc_kind }

type dir_meta = { dir_install : bool }
type silo_meta = unit

type custom_data = exn
type custom_meta = { custom_data : custom_data }

type meta =
  | Base of base_meta
  | Unit of unit_meta
  | Lib of lib_meta
  | Bin of bin_meta
  | Pkg of pkg_meta
  | Run of run_meta
  | Doc of doc_meta
  | Dir of dir_meta
  | Silo of silo_meta
  | Custom of custom_meta

type +'a t =
  { p_kind : kind;
    p_name : string;
    p_cond : bool As_conf.value;
    p_deps : kind t list;
    p_args : kind t -> As_args.t;
    p_actions : kind t -> As_action.t list;
    p_meta : meta; }
constraint 'a = [< kind ]

(* Coercing *)

let coerce (#kind as k) ({p_kind} as p) =
  if p.p_kind = k then p else
  invalid_arg (err_coerce (kind_to_string p.p_kind) (kind_to_string k))

let coerce_if (#kind as k) ({p_kind} as p) =
  if p.p_kind = k then Some p else None

(* Basic fields *)

let name p = p.p_name
let kind p = p.p_kind
let cond p = p.p_cond
let deps p = p.p_deps
let args p = p.p_args (p :> kind t)   (* FIXME memoize *)
let actions p = p.p_actions (p :> kind t) (* FIXME memoize *)
let meta p = p.p_meta

(* Derived fields *)

let products p =
  let outputs = List.map As_action.outputs (actions p) in
  let add acc outputs = As_conf.(const List.rev_append $ outputs $ acc) in
  let rev_outputs = List.fold_left add (As_conf.const []) outputs in
  As_conf.(const List.rev $ rev_outputs)

(* Comparing *)

let id p = kind_to_string (kind p) ^ (name p)
let equal p p' = id p = id p'
let compare p p' = Pervasives.compare (id p) (id p')

(* Component list operations *)

let keep pred ps =
  let keep acc p = if pred p then p :: acc else acc in
  List.rev (List.fold_left keep [] ps)

let keep_map fn ps =
  let add acc p = match fn p with None -> acc | Some v -> v :: acc in
  List.rev (List.fold_left add [] ps)

let keep_kind kind ps = keep_map (coerce_if kind) ps
let keep_kinds kinds ps = keep (fun p -> List.mem (kind p) kinds) ps

let to_set l =
  let rec add seen acc = function
  | [] -> List.rev acc
  | p :: ps ->
      let name = id p in
      if As_string.Set.mem name seen then add seen acc ps else
      add (As_string.Set.add name seen) (p :: acc) ps
  in
  add As_string.Set.empty [] l

(* Part constructor *)

let create ?(cond = As_conf.true_) ?(args = fun _ -> As_args.empty)
    ?(deps = []) ?(actions = fun _  -> []) name kind meta =
  let deps = to_set deps in
  { p_kind = (kind :> kind);
    p_name = name;
    p_cond = cond;
    p_deps = (deps :> kind t list);
    p_actions = (actions :> kind t -> As_action.t list);
    p_args = args;
    p_meta = meta }

let add_deps_args deps args u = (* N.B. this slows downs things a lot. *)
  let p_deps = to_set (u.p_deps @ deps) in
  let p_args _ = As_args.(@@@) (u.p_args (u :> kind t)) args in
  { u with p_deps; p_args }

(* Meta data accessors and part filters *)

let part_filter kind get_part_subkind part_subkind p =
  match coerce_if kind p with
  | None -> None
  | Some part as p -> if get_part_subkind part = part_subkind then p else None

let unit_get_meta p = match p.p_meta with Unit m -> m | _ -> assert false
let unit_kind p = (unit_get_meta p).unit_kind
let unit_src_dir p = (unit_get_meta p).unit_src_dir
let unit_js p = part_filter `Unit unit_kind `Js p
let unit_ocaml p = match coerce_if `Unit p with
| None -> None
| Some part as p -> match unit_kind part with `OCaml _ -> p | _ -> None

let unit_c p = match coerce_if `Unit p with
| None -> None
| Some part as p -> match unit_kind part with `C _ -> p | _ -> None

let lib_get_meta p = match p.p_meta with Lib l -> l | _ -> assert false
let lib_kind p = (lib_get_meta p).lib_kind
let lib_byte p = (lib_get_meta p).lib_byte
let lib_native p = (lib_get_meta p).lib_native
let lib_native_dynlink p = (lib_get_meta p).lib_native_dynlink
let lib_ocaml p = part_filter `Lib lib_kind `OCaml p
let lib_ocaml_pp p = part_filter `Lib lib_kind `OCaml_pp p
let lib_c p = part_filter `Lib lib_kind `C p

let bin_get_meta p = match p.p_meta with Bin b -> b | _ -> assert false
let bin_kind p = (bin_get_meta p).bin_kind
let bin_byte p = (bin_get_meta p).bin_byte
let bin_native p = (bin_get_meta p).bin_native
let bin_js p = (bin_get_meta p).bin_js
let bin_ocaml p = part_filter `Bin bin_kind `OCaml p
let bin_ocaml_toplevel p = part_filter `Bin bin_kind `OCaml_toplevel p
let bin_c p = part_filter `Bin bin_kind `C p

let pkg_get_meta p = match p.p_meta with Pkg p -> p | _ -> assert false
let pkg_kind p = (pkg_get_meta p).pkg_kind
let pkg_ocaml p = part_filter `Pkg pkg_kind `OCaml p
let pkg_c p = part_filter `Pkg pkg_kind `C p

let run_get_meta p = match p.p_meta with Run r -> r | _ -> assert false
let run_run_dir p = (run_get_meta p).run_dir

let doc_get_meta p = match p.p_meta with Doc d -> d | _ -> assert false
let doc_kind p = (doc_get_meta p).doc_kind

let dir_get_meta p = match p.p_meta with Dir d -> d | _ -> assert false
let dir_install p = (dir_get_meta p).dir_install
let dir_kind p = match p.p_name with
| "lib" -> `Lib | "bin" -> `Bin | "sbin" -> `Sbin | "toplevel" -> `Toplevel
| "share" -> `Share | "share_root" -> `Share_root | "etc" -> `Etc
| "doc" -> `Doc | "misc" -> `Misc | "stublibs" -> `Stublibs | "man" -> `Man
| k -> `Other k

let silo_get_meta p = match p.p_meta with Silo s -> s | _ -> assert false

let custom_get_meta p = match p.p_meta with Custom m -> m | _ -> assert false
let custom_data p = (custom_get_meta p).custom_data

(* Specific parts. *)

module Base = struct
  let create ?cond ?args ?deps name actions =
    create ?cond ?args ?deps ~actions name `Base (Base ())
end

module Unit = struct

  (* Metadata *)

  type ocaml_interface = unit_ocaml_interface
  type ocaml_unit = unit_ocaml_unit
  type c_unit = unit_c_unit
  type kind = unit_kind

  let meta kind src_dir = Unit { unit_kind = kind; unit_src_dir = src_dir }
  let get_meta = unit_get_meta
  let kind = unit_kind
  let src_dir = unit_src_dir

  (* Rules *)

(*
  let unit_file fext env u =
    As_path.(as_rel (As_env.build_dir env // (file (name u)) + fext))

  let unit_args u =
    let pkgs = keep_kind `Pkg (deps u) in
    let pkgs_args = As_args.concat (List.map args pkgs) in
    let libs = keep_kind `Lib (deps u) in
    let lib_args lib =
      let cma = List.filter (As_product.has_ext `Cma) (products lib) in
      let cmxa = List.filter (As_product.has_ext `Cmxa) (products lib) in
      match lib_kind lib with
      | `OCaml ->
          let inc ctxs a = As_product.dirname_to_args ~pre:["-I"] ctxs a in
          let prod ctxs a = As_product.target_to_args ctxs a in
          let cma_inc = List.map (inc [`Compile `Byte; `Link `Byte]) cma in
          let cma_prod = List.map (prod [`Link `Byte]) cma in
          let cmxa_inc = List.map (inc [`Compile `Native;`Link `Native]) cmxa in
          let cmxa_prod = List.map (prod [`Link `Native]) cmxa in
          As_args.concat (cma_inc @ cma_prod @ cmxa_inc @ cmxa_prod)
      | `OCaml_pp ->
          let prod a = As_product.target_to_args [`Pp `Byte; `Pp `Native] a in
          As_args.concat (List.map prod cma)
      | `C ->
          As_args.empty
    in
    As_args.(args env u @@@ pkgs_args @@@ concat (List.map lib_args libs))

  let link_src fext env u =
    let cond = cond u in
    let dst = unit_file fext env u in
    let src = As_path.(as_rel (src_dir env u / (basename dst))) in
    As_action.link ~cond env ~src ~dst

  let js_rules u env = [ link_src `Js u env ]
  let js_args u env = As_args.empty

  let c_compile env u =
    let context = `Compile `C in
    let product p = `File p, cond u in
    let c = unit_file `C env u in
    let o = unit_file `O env u in
    let inputs = [ product c ] in
    let outputs = [ product o ] in
    let compile args = As_env.ocamlc env :: args @ [ As_path.basename c ] in
    let action = [ unit_args env u, compile ] in
    As_action.create ~context ~inputs ~outputs ~action

  let rec c_rules c_unit env u = match c_unit with
  | `H -> [ link_src `H env u ]
  | `C -> [ link_src `C env u; c_compile env u; ]
  | `Both -> c_rules `H env u @ c_rules `C env u

  let ocamlpp_ext fext ctx =
    let kind = match fext with `Ml -> "cml" | `Mli -> "cmli" in
    let ctx = match ctx with `Byte -> "byte" | `Native -> "native" in
    `Ext (str "%s-%s" kind ctx)

  let ocaml_pp fext ctx env u =
    let src = unit_file (fext :> As_path.ext) env u in
    let src_pped = unit_file (ocamlpp_ext fext ctx) env u in
    match As_env.ocaml_pp env with
    | None -> As_action.link ~cond:(cond u) env ~src ~dst:src_pped
    | Some pp ->
        let context = (`Pp ctx :> As_context.t) in
        let product p = `File p, cond u in
        let inputs = [ product src ] in
        let outputs = [ product src_pped ] in
        let pp args =
          pp :: args @
          [ As_path.to_string src; ">"; As_path.to_string src_pped ]
        in
        let action = [ unit_args env u, pp ] in
        As_action.create ~context ~inputs ~outputs ~action

  let ocaml_compile_mli env u =
    let context = `Compile `Intf in
    let product p = `File p, cond u in
    let cmli = unit_file (ocamlpp_ext `Mli `Byte) env u in
(*    let mli_dep = unit_file `Mli_dep env u in (* defined by parent *) TODO *)
    let cmi = unit_file `Cmi env u in
    let inputs = [ product cmli; (* product mli_dep *) ] in
    let outputs = [ product cmi ] in
    let compile args =
      As_env.ocamlc env :: args @ [ "-c"; "-intf"; (As_path.to_string cmli); ]
    in
    let action = [ unit_args env u, compile ] in
    As_action.create ~context ~inputs ~outputs ~action

  let ocaml_compile_ml_byte env u =
    let context = `Compile `Byte in
    let product p = `File p, As_conf.(cond u &&& value ocaml_byte) in
    let has_mli = match kind u with `OCaml (`Both, _) -> true | _ -> false in
    let ml_dep = unit_file `Ml_dep env u in
    let cml = unit_file (ocamlpp_ext `Ml `Byte) env u in
    let cmi = unit_file `Cmi env u in
    let cmo = unit_file `Cmo env u in
    let inputs = [ product cml; product ml_dep ] in
    let inputs = if not has_mli then inputs else (product cmi) :: inputs in
    let outputs = [ product cmo ] in
    let outputs = if has_mli then outputs else (product cmi) :: outputs in
    let compile args =
      As_env.ocamlc env :: args @ [ "-c"; "-impl"; (As_path.to_string cml) ]
    in
    let action = [ unit_args env u, compile ] in
    As_action.create ~context ~inputs ~outputs ~action

  let ocaml_compile_ml_native env u =
    let context = `Compile `Native in
    let product p = `File p, As_conf.(cond u &&& value ocaml_native) in
    let has_mli = match kind u with `OCaml (`Both, _) -> true | _ -> false in
    let cml = unit_file (ocamlpp_ext `Ml `Native) env u in
    let cmi = unit_file `Cmi env u in
    let ml_dep = unit_file `Ml_dep env u in
    let cmx = unit_file `Cmx env u in
    let inputs = [ product cml; product ml_dep ] in
    let inputs = if not has_mli then inputs else (product cmi) :: inputs in
    let outputs = [ product cmx ] in
    let outputs = if has_mli then outputs else (product cmi) :: outputs in
    let compile args =
      As_env.ocamlopt env :: args @ [ "-c"; "-impl"; (As_path.to_string cml); ]
    in
    let action = [ unit_args env u, compile ] in
    As_action.create ~context ~inputs ~outputs ~action

  let rec ocaml_rules unit env u = match unit with
  | `Mli ->
      [ link_src `Mli env u;
        ocaml_pp `Mli `Byte env u;
        ocaml_compile_mli env u; ]
  | `Ml ->
      [ link_src `Ml env u;
        ocaml_pp `Ml `Byte env u;
        ocaml_pp `Ml `Native env u;
        ocaml_compile_ml_byte env u;
        ocaml_compile_ml_native env u; ]
  | `Both ->
      ocaml_rules `Mli env u @ ocaml_rules `Ml env u
*)
  let actions p = []
(*
    let u = coerce `Unit p in
    match kind u with
    | `Js -> js_rules env u
    | `C unit -> c_rules unit env u
    | `OCaml (unit, _) -> ocaml_rules unit env u
*)

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps
      ?(src_dir = As_path.current) name kind =
    let meta = meta kind src_dir in
    let args _ = args in
    create ?cond ~args ?deps ~actions name `Unit meta

  let of_base ~src_dir kind p =
    { p with p_kind = `Unit;
             p_meta = meta kind src_dir }

  let check_set s = s (* TODO *)

  (* Part filters *)

  let ocaml = unit_ocaml
  let c = unit_c
  let js = unit_js
end

module Lib = struct

  (* Metadata *)

  type kind = lib_kind

  let meta ?(byte = true) ?(native = true) ?(native_dynlink = true) kind =
    Lib { lib_kind = kind; lib_byte = byte; lib_native = native;
          lib_native_dynlink = native_dynlink }

  let get_meta = lib_get_meta
  let kind = lib_kind
  let byte = lib_byte
  let native = lib_native
  let native_dynlink = lib_native_dynlink

  (* Rules *)

(*
  let lib_file fext l =
    As_path.(as_rel (As_env.build_dir env // (file (name l)) + fext))

  let lib_args l =
    let pkgs = keep_kind `Pkg (deps l) in
    let pkgs_args = As_args.concat (List.map args pkgs) in
    As_args.(args env l @@@ pkgs_args)

  let c_archive_shared units l =
    let context = `Archive `C_shared in (* FIXME this is also `Archive `C *)
    let product p = `File p, cond l in
    let units_prods = List.(flatten (map producs units)) in
    let units_o = List.(filter (As_product.has_ext `O) units_prods) in
    let a = lib_file `A env l in
    let so = lib_file `So env l in
    let inputs = units_o in
    let outputs = [ product a; product so ] in
    let units_o = List.map As_product.raw_path units_o in
    let archive args =
      As_env.ocamlmklib env :: args @
      [ "-o"; As_path.(basename (chop_ext so));] @ units_o
    in
    let action = [ lib_args env l, archive ] in
    As_action.create ~context ~inputs ~outputs ~action

  let c_rules units env l = [ c_archive_shared units env l ]

  let ocaml_archive_byte units env l =
    let context = `Archive `Byte in
    let product p = `File p, As_conf.(cond l &&& value ocaml_byte) in
    let units_prods = List.(flatten (map (products env) units)) in
    let units_cmo = List.(filter (As_product.has_ext `Cmo) units_prods) in
    let cma = lib_file `Cma env l in
    let inputs = units_cmo in
    let outputs = [ product cma ] in
    let units_cmo = List.map As_product.raw_path units_cmo in
    let archive args =
      As_env.ocamlc env :: args @
      [ "-a"; "-o"; As_path.to_string cma ] @ units_cmo
    in
    let action = [ lib_args env l, archive ] in
    As_action.create ~context ~inputs ~outputs ~action

  let ocaml_archive_native units env l =
    let context = `Archive `Native in
    let product p = `File p, As_conf.(cond l &&& value ocaml_native) in
    let units_prods = List.(flatten (map (products env) units)) in
    let units_cmx = List.(filter (As_product.has_ext `Cmx) units_prods) in
    let cmxa = lib_file `Cmxa env l in
    let inputs = units_cmx in
    let outputs = [ product cmxa ] in
    let units_cmx = List.map As_product.raw_path units_cmx in
    let archive args =
      As_env.ocamlopt env :: args @
      [ "-a"; "-o"; As_path.to_string cmxa ] @ units_cmx
    in
    let action = [ lib_args env l, archive ] in
    As_action.create ~context ~inputs ~outputs ~action

  let ocaml_archive_shared units env l =
    let context = `Archive `Native in
    let product p = `File p, As_conf.(cond l &&& value ocaml_native_dynlink) in
    let units_prods = List.(flatten (map (products env) units)) in
    let units_cmx = List.(filter (As_product.has_ext `Cmx) units_prods) in
    let cmxs = lib_file `Cmxs env l in
    let inputs = units_cmx in
    let outputs = [ product cmxs ] in
    let units_cmx = List.map As_product.raw_path units_cmx in
    let archive args =
      As_env.ocamlopt env :: args @
      [ "-shared"; "-o"; As_path.to_string cmxs ] @ units_cmx
    in
    let action = [ lib_args env l, archive ] in
    As_action.create ~context ~inputs ~outputs ~action

  let ocaml_rules units env l =
    (*  FIXME: check if there are C units and use directly ocamlmklib *)
    let byte = if byte l then [ ocaml_archive_byte units env l ] else [] in
    let nat = if native l then [ ocaml_archive_native units env l ] else [] in
    let dyn =
      if native_dynlink l then [ ocaml_archive_shared units env l ] else []
    in
    List.concat [ byte; nat; dyn; ]

  let ocaml_pp_rules units env l = [ ocaml_archive_byte units env l ]
*)
  let actions units p = []
(*
    let l = coerce `Lib p in
    let build_dir = As_path.dir (kind_to_string `Lib ^ "-" ^ (name l)) in
    let env = As_env.push_build_dir env build_dir in
    let units_rules = List.(flatten (map (actions env) units)) in
    let mkdir_rule = As_action.mkdir env ~dir:(As_env.build_dir env) in
    match kind l with
    | `C -> mkdir_rule :: units_rules @ c_rules units env p
    | `OCaml -> mkdir_rule :: units_rules @ ocaml_rules units env p
    | `OCaml_pp -> mkdir_rule :: units_rules @ ocaml_pp_rules units env p
*)

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps:(ds = []) ?byte ?native
      ?native_dynlink name kind (units : [< `Unit] t list)  =
    let meta = meta ?byte ?native ?native_dynlink kind in
    let units = List.map (add_deps_args ds args) units in
    let deps = List.flatten (List.map deps units) in
    let args _  = args in
    let actions = actions units in
    create ?cond ~args ~deps ~actions name `Lib meta

  let of_base ?byte ?native ?native_dynlink kind p =
    let meta = meta ?byte ?native ?native_dynlink kind in
    { p with p_kind = `Lib; p_meta = meta }

  (* Part filters *)

  let get_meta p = match p.p_meta with Lib l -> l | _ -> assert false
  let kind p = (get_meta p).lib_kind
  let byte p = (get_meta p).lib_byte
  let native p = (get_meta p).lib_native
  let native_dynlink p = (get_meta p).lib_native_dynlink

  let ocaml = lib_ocaml
  let ocaml_pp = lib_ocaml_pp
  let c = lib_c
end

module Bin = struct

  (* Metadata *)

  type kind = bin_kind

  let meta ?(byte = true) ?(native = true) ?(js = true) bin_kind =
    Bin { bin_kind; bin_byte = byte; bin_native = native; bin_js = js }

  let get_meta = bin_get_meta
  let kind = bin_kind
  let byte = bin_byte
  let native = bin_native
  let js = bin_js

  let get_meta p = match p.p_meta with Bin b -> b | _ -> assert false
  let byte p = (get_meta p).bin_byte
  let native p = (get_meta p).bin_native
  let js p = (get_meta p).bin_js

  (* Actions *)

  let ocaml_actions units p = []

  let actions units p = []
(*
    let b = coerce `Bin p in
    let build_dir = As_path.dir (kind_to_string `Bin ^ "-" ^ (name b)) in
    let env = As_env.push_build_dir env build_dir in
    let units_actions = List.(flatten (map actions units)) in
    units_actions @ ocaml_actions units p
*)

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps:(ds = []) ?byte ?native
      ?js name kind units =
    let meta = meta ?byte ?native ?js kind in
    let deps = ds @ List.flatten (List.map deps units) in
    let units = List.map (add_deps_args deps args) units in
    let args _ = args in
    let actions = actions units in
    create ?cond ~args ~deps ~actions name `Bin meta

  let of_base ?byte ?native ?js kind p =
    let meta = meta ?byte ?native ?js kind in
    { p with p_kind = `Bin; p_meta = meta }

  (* As build commands *)

(*
  let cmd ?(args = As_args.empty) ?kind bin args' =
    args, fun args -> "TODO EXEC NAME" :: (args' args)
*)

  (* Package filters *)

  let ocaml = bin_ocaml
  let ocaml_toplevel = bin_ocaml_toplevel
  let c = bin_c
end

module Pkg = struct

  (* Metadata *)

  type kind = pkg_kind

  let meta kind = Pkg { pkg_kind = kind; }
  let get_meta = pkg_get_meta
  let kind = pkg_kind

  (* Create *)

  type ocaml_lookup = [ `OCamlfind ]
  type c_lookup = [ `Pkg_config ]
  type spec = [ `C of c_lookup | `OCaml of ocaml_lookup ]

  let ocamlfind_lookup name args _  = args
(*
    As_args.(As_env.ocamlfind_pkgs env [name] @@@ args)
*)

  let pkg_config_lookup name args _ = args
(*
    As_args.(As_env.pkg_config env [name] @@@ args)
*)

  let create ?cond ?(args = As_args.empty) name spec =
    let kind, args = match spec with
    | `OCaml `OCamlfind -> `OCaml, ocamlfind_lookup name args
    | `C `Pkg_config -> `C, pkg_config_lookup name args
    in
    let meta = meta kind in
    create ?cond ~args name `Pkg meta

  let of_base kind p =
    let meta = meta kind in
    { p with p_kind = `Pkg; p_meta = meta }

  (* Part filters *)

  let ocaml = pkg_ocaml
  let c = pkg_c
end

module Run = struct

  (* Metadata *)

  let meta ?(run_dir = As_path.current) () = Run { run_dir }
  let get_meta = run_get_meta
  let run_dir = run_run_dir

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps ?run_dir name cmds =
    let meta = meta ?run_dir () in
    let args _ = args in
    create ?cond ~args ?deps name `Run meta

  let of_base ?run_dir p =
    let meta = meta ?run_dir () in
    { p with p_kind = `Run; p_meta = meta }
end

module Doc = struct

  (* Metadata *)

  type kind = doc_kind

  let meta ?(kind = `OCamldoc) () = Doc { doc_kind = kind }
  let get_meta = doc_get_meta
  let kind = doc_kind

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps ?keep ?kind name ps =
    let meta = meta ?kind () in
    let args _ = args in
    create ?cond ~args ?deps name `Doc meta

  let of_base ?kind p =
    let meta = meta ?kind () in
    { p with p_kind = `Doc; p_meta = meta }

  (* Documentation filters *)

  let default _ = failwith "TODO"
  let dev _ = failwith "TODO"
end

module Dir = struct

  (* Metadata *)

  type kind = [ `Lib | `Bin | `Sbin | `Toplevel | `Share | `Share_root
              | `Etc | `Doc | `Misc | `Stublibs | `Man | `Other of string ]

  let name_of_kind = function
  | `Lib -> "lib" | `Bin -> "bin" | `Sbin -> "sbin" | `Toplevel -> "toplevel"
  | `Share -> "share" | `Share_root -> "share_root" | `Etc -> "etc"
  | `Doc -> "doc" | `Misc -> "misc" | `Stublibs -> "stublibs" | `Man -> "man"
  | `Other o -> o

  let meta ?(install = true) () = Dir { dir_install = install }

  let get_meta = dir_get_meta
  let install = dir_install
  let kind = dir_kind

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps ?keep ?install kind ps =
    let meta = meta ?install () in
    let args _ = args in
    create ?cond ~args ?deps (name_of_kind kind) `Dir meta

  let of_base ?install p =
    let meta = meta ?install () in
    { p with p_kind = `Dir; p_meta = meta }

  (* Product filters *)

  let default _ = failwith "TODO"
end

module Silo = struct

  (* Metadata *)

  let meta () = Silo ()
  let get_meta = silo_get_meta

  (* Create *)

  let create ?cond ?(args = As_args.empty) ?deps name ps =
    let meta = meta () in
    let args _ = args in
    create ?cond ~args ?deps name `Silo meta

  let of_base p =
    let meta = meta () in
    { p with p_kind = `Silo; p_meta = meta }
end

module Custom = struct

  (* Metadata *)

  type data = custom_data

  let key (type s) () =
    (* universal type see http://mlton.org/UniversalType *)
    let module M = struct exception E of s option end in
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  let meta m = Custom { custom_data = m }
  let get_meta = custom_get_meta
  let data = custom_data

  (* Create *)

  let of_base m p =
    let meta = meta m in
    { p with p_kind = `Custom; p_meta = meta }
end
