open Sexplib.Std

type t = {
  foo: int;
} with sexp

let x = Xmlm.input
