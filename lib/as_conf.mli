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

(** Build configuration.

    See {!Assemblage.Conf} for documentation. *)

(** {1 Configuration values} *)

type 'a value

val const : 'a -> 'a value
val app : ('a -> 'b) value -> 'a value -> 'b value
val ( $ ) : ('a -> 'b) value -> 'a value -> 'b value

(** {1 Configuration keys} *)

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer
type 'a key

module Key : sig
  type t = V : 'a key -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val name : 'a key -> string
  val public : 'a key -> bool
  val converter : 'a key -> 'a value converter
  val default_value : 'a key -> 'a value
  val doc : 'a key -> string option
  val docv : 'a key -> string option
  val docs : 'a key -> string option
end

val key : ?public:bool -> ?docs:string -> ?docv:string -> ?doc:string ->
  string -> 'a converter -> 'a value -> 'a key

val value : 'a key -> 'a value

(** {2 Configuration key value converters} *)

val bool : bool converter
val int : int converter
val string : string converter
val path : As_path.t converter
val abs_path : As_path.abs converter
val rel_path : As_path.rel converter
val enum : (string * 'a) list -> 'a converter
val version : (int * int * int * string option) converter

(** {1 Configurations} *)

type t
val empty : t
val is_empty : t -> bool
val add : t -> 'a key -> t
val ( + ) : t -> 'a key -> t
val set : t -> 'a key -> 'a value -> t
val merge : t -> t -> t
val ( ++ ) : t -> t -> t
val find : t -> 'a key -> 'a value option
val get : t -> 'a key -> 'a value
val ( @ ) : t -> 'a key -> 'a value
val parse : t -> t Cmdliner.Term.t
(*
val fold : ('a -> Key.t -> 'a) -> 'a -> t -> 'a
val iter : (Key.t -> unit) -> t -> unit
val exists : (Key.t -> bool) -> t -> bool
val keep : (Key.t -> bool) -> t -> t
val subset : t -> t -> bool
val diff : t -> t -> t
val keys : t -> Key.t list
val name_dups : t -> Key.t list
*)

val eval : t -> 'a value -> 'a
val deps : 'a value -> t

(** {1:builtin Built-in configuration keys} *)

val builtin_base : t

(** {2:builtin_build Build property keys} *)

val debug : bool key
val profile : bool key
val test : bool key
val doc : bool key
val jobs : int key

val builtin_build_props : t

(** {2:build_directories Build directories} *)

val root_dir : As_path.t key
val build_dir : As_path.rel key
val product_dir : As_path.rel key

val builtin_build_dirs : t

(** {2:ocaml_system OCaml system keys} *)

val ocaml_native_tools : bool key
val ocaml_version : (int * int * int * string option) key
val ocaml_byte : bool key
val ocaml_native : bool key
val ocaml_native_dynlink : bool key
val ocaml_js : bool key
val ocaml_annot : bool key
val ocaml_warn_error : bool key
val ocaml_pp : string key
val ocamlc : string key
val ocamlopt : string key
val js_of_ocaml : string key
val ocamldep : string key
val ocamlmklib : string key
val ocamllex : string key
val ocamlyacc : string key
val ocaml : string key
val ocamlrun : string key
val ocamldebug : string key
val ocamlprof : string key
val ocamlfind : string key

val builtin_ocaml : t

(** {2 Basic system utilities} *)

val echo : string key
val ln : string key
val cp : string key
val mkdir : string key
val cat : string key
val make : string key

val builtin_base_utils : t

(** {2 C system keys} *)

val cc : string key
val pkg_config : string key

val builtin_c : t

(** {2 Machine information keys} *)

val uname : string key
val os : string key
val arch : string key

val builtin_machine_info : t
