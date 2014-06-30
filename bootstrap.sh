#!/bin/sh

set -ex

ocamlbuild lib/shell.cmo lib/git.cmo lib/build_env.cmo \
    lib/project.cmo lib/ocamlfind.cmo lib/opam.cmo \
    lib/makefile.cmo lib/tools.cmo \
    -package cmdliner -package opam -package ocamlgraph \
    -package compiler-libs

ocamlc \
    $(ocamlfind query -r unix cmdliner opam compiler-libs.toplevel \
      -predicates byte -format "-I %d %a")  \
    -I _build/lib shell.cmo git.cmo project.cmo ocamlfind.cmo opam.cmo \
    makefile.cmo build_env.cmo tools.cmo bin/configure.ml -o configure.boot

./configure.boot --disable-auto-load-tools -I _build/lib \
    --enable-warn-error --enable-test
