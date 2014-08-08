#!/bin/sh

set -ex

ocamlbuild -pkgs cmdliner,ocamlgraph,compiler-libs \
    -pp camlp4o,`ocamlfind query optcomp ocamlgraph -r -predicates \
      syntax,preprocessor -format "%d/%a"` \
    lib/as_shell.cmo lib/as_git.cmo \
    lib/as_features.cmo lib/as_flags.cmo lib/as_resolver.cmo \
    lib/as_action.cmo \
    lib/as_build_env.cmo lib/as_project.cmo lib/as_ocamlfind.cmo \
    lib/as_OCaml.cmo lib/as_cmd.cmo \
    lib/as_opam.cmo lib/as_makefile.cmo lib/assemblage.cmo

ocamlc -linkall \
    `ocamlfind query -r unix cmdliner compiler-libs.toplevel ocamlgraph \
      -predicates byte -format "-I %d %a"`  \
    -I _build/lib as_shell.cmo as_git.cmo \
    as_features.cmo as_flags.cmo as_resolver.cmo \
    as_action.cmo as_project.cmo as_ocamlfind.cmo as_OCaml.cmo \
    as_opam.cmo as_makefile.cmo as_build_env.cmo as_cmd.cmo \
    assemblage.cmo \
    bin/configure.ml -o configure.boot

ocamlc -linkall \
    `ocamlfind query -r unix cmdliner compiler-libs.toplevel ocamlgraph \
      -predicates byte -format "-I %d %a"`  \
    -I _build/lib as_shell.cmo as_git.cmo \
    as_features.cmo as_flags.cmo as_resolver.cmo \
    as_action.cmo as_project.cmo as_ocamlfind.cmo as_OCaml.cmo \
    as_opam.cmo as_makefile.cmo as_build_env.cmo as_cmd.cmo assemblage.cmo \
    bin/tool.ml -o assemblage.boot


./assemblage.boot --disable-auto-load -I _build/lib \
  --enable-warn-error --disable-test

#./configure.boot --disable-auto-load -I _build/lib \
#    --enable-warn-error --disable-test
