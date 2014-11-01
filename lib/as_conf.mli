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
val true_ : bool value
val false_ : bool value
val neg : bool value -> bool value
val ( &&& ) : bool value -> bool value -> bool value
val ( ||| ) : bool value -> bool value -> bool value
val pick_if : bool value -> 'a value -> 'a value -> 'a value

(** {1 Configuration value converters} *)

type 'a parser = string -> [ `Error of string | `Ok of 'a ]
type 'a printer = Format.formatter -> 'a -> unit
type 'a converter = 'a parser * 'a printer

val parser : 'a converter -> 'a parser
val printer : 'a converter -> 'a printer

(** {1 Configuration keys} *)

type 'a key

module Key : sig
  type t = V : 'a key -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val id : 'a key -> int
  val name : 'a key -> string
  val public : 'a key -> bool
  val converter : 'a key -> 'a converter
  val default : 'a key -> 'a value
  val doc : 'a key -> string option
  val docv : 'a key -> string option
  val docs : 'a key -> string option

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
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
val set : t -> 'a key -> 'a value -> t
val merge : t -> t -> t
val find : t -> 'a key -> 'a value option
val get : t -> 'a key -> 'a value
val domain : t -> Key.Set.t
val of_keys : Key.Set.t -> t
val eval : t -> 'a value -> 'a
val deps : 'a value -> Key.Set.t
val pp : Format.formatter -> t -> unit

(** {1 Configuration error messages} *)

val pp_key_dup : Format.formatter -> Key.t -> unit

(** {1 Built-in configuration keys} *)

(** {1 Project keys} *)

val project_version : string key
val docs_project : string
val doc_project : string

(** {2 Machine information keys} *)

val docs_machine_information : string
val doc_machine_information : string

val uname : string key
val host_os : string key
val host_arch : string key
val host_word_size : int key
val target_os : string key
val target_arch : string key
val target_word_size : int key

(** {2 Build directory keys} *)

val docs_build_directories : string
val doc_build_directories : string

val root_dir : As_path.t key
val build_dir : As_path.rel key
val product_dir : As_path.rel key

(** {2 Build property keys} *)

val docs_build_properties : string
val doc_build_properties : string

val debug : bool key
val profile : bool key
val warn_error : bool key
val test : bool key
val doc : bool key
val jobs : int key

(** {2 OCaml system keys} *)

val docs_ocaml_system : string
val doc_ocaml_system : string

val ocaml_native_tools : bool key
val ocaml_version : (int * int * int * string option) key
val ocaml_byte : bool key
val ocaml_native : bool key
val ocaml_native_dynlink : bool key
val ocaml_build_ast : bool key
val ocaml_js : bool key
val ocaml_annot : bool key
val ocaml_dumpast : string key
val ocamlc : string key
val ocamlopt : string key
val js_of_ocaml : string key
val ocamldep : string key
val ocamlmklib : string key
val ocamldoc : string key
val ocamllex : string key
val ocamlyacc : string key
val ocaml : string key
val ocamlrun : string key
val ocamldebug : string key
val ocamlprof : string key
val ocamlfind : string key
val opam : string key
val opam_installer : string key
val opam_admin : string key

(** {2 C system keys} *)

val docs_c_system : string
val doc_c_system : string

val cc : string key
val pkg_config : string key

(** {2 System utility keys} *)

val docs_system_utilities : string
val doc_system_utilities : string

val ln : string key
val cp : string key
val mv : string key
val cd : string key
val rm : string key
val rmdir : string key
val mkdir : string key
val cat : string key
val make : string key
