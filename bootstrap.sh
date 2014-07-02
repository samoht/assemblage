#!/bin/sh

set -ex

ocamlbuild -pkgs cmdliner,opam,ocamlgraph,compiler-libs \
    -pp camlp4o,`ocamlfind query optcomp -r -predicates syntax,preprocessor -format "%d/%a"` \
    lib/shell.cmo lib/git.cmo lib/flags.cmo lib/resolver.cmo \
    lib/feature.cmo lib/action.cmo \
    lib/build_env.cmo lib/project.cmo lib/ocamlfind.cmo lib/OCaml.cmo \
    lib/opam.cmo lib/makefile.cmo lib/assemblage.cmo

ocamlc -linkall \
    `ocamlfind query -r unix cmdliner opam compiler-libs.toplevel \
      -predicates byte -format "-I %d %a"`  \
    -I _build/lib shell.cmo git.cmo flags.cmo resolver.cmo \
    feature.cmo action.cmo project.cmo ocamlfind.cmo OCaml.cmo \
    opam.cmo makefile.cmo build_env.cmo assemblage.cmo \
    bin/configure.ml -o configure.boot

./configure.boot --disable-auto-load -I _build/lib \
    --enable-warn-error --enable-test
