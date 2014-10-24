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

open Assemblage
open Assemblage.Private

module Cli = struct

  let value_converter_of_converter (parse, _) =
    let parse s = match parse s with
    | `Ok v -> `Ok (Conf.const v) | `Error _ as e -> e
    in
    let print = Fmt.nop (* config needed be accurate, use describe cmd *) in
    parse, print

  let term_of_conf c =
    let add (Conf.Key.V kt as k) (names, acc) =
      if As_string.Set.mem (Conf.Key.name kt) names
      then (Log.warn "%a" Conf.pp_key_dup k; (names, acc))
      else
      let names' = As_string.Set.add (Conf.Key.name kt) names in
      let v = Conf.Key.default kt in
      let c = value_converter_of_converter (Conf.Key.converter kt) in
      let doc = Conf.Key.doc kt in
      let docs = Conf.Key.docs kt in
      let docv = Conf.Key.docv kt in
      let i = Cmdliner.Arg.info [Conf.Key.name kt] ?doc ?docv ?docs in
      let opt = Cmdliner.Arg.(value (opt c v & i)) in
      let acc' = Cmdliner.Term.(pure Conf.set $ acc $ pure kt $ opt) in
      (names', acc')
    in
    let acc = (As_string.Set.empty, Cmdliner.Term.pure c) in
    snd (Conf.Key.Set.fold add (Conf.domain c) acc)
end
