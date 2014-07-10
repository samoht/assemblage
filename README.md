## Assemblage

> __Assemblage__ is an artistic process. In the visual arts, it
  consists of making three-dimensional or two-dimensional artistic
  compositions by putting together found objects.
  [wikipedia](http://en.wikipedia.org/wiki/Assemblage_(art))

The *Assemblage* toolbox provides an API and a set of binaries to
configure, manage, and use OCaml projects.

### Status

[![Build Status](https://travis-ci.org/samoht/assemblage.svg?branch=master)](https://travis-ci.org/samoht/assemblage)

The library can be used to describe OCaml projects with static dependencies.

A projects description consists of a list of libraries, binaries and/or toplevels.
Each containing a list of compilation units with precise (and static) dependency
relationships.

Using a project description, the tools can generate:

- a `Makefile` to build the project;
- a `.install` to install the project; and
- a `META` file to use the project.

### Dependency Tracking

Multiple kinds of dependencies are currently supported:

- a single directory, with a collection of files to sort to form a
  library (using ocamldep)
- local libraries, defined in the same project
- local syntax extensions, defined in the same project (using `camlp4o`)
- global ocamlfind libraries
- global ocamlfind syntax extensions (using `camlp4o`)

### Project Flags

Every project is parametrized by a set of flags, which will determine sub-parts
of the project to be built (and installed) or not. The flag can be enabled or
disabled either programmatically (ie. in the project description itself) or
passed on the command-line using an integrated `Cmdliner` interface.

The package also installs a `opam-configure` helper scripts, which can be used
to configure an OCaml project using the state of OPAM in the user computer.

### Examples

There are currently two tools installed by *Assemblage*: `configure.ml` and
`describe.ml`. `configure.ml` reads a project description and generates the
files to build, install and use the project. `describe.ml` displays a summary
of the project:

```shell
$ thomas@piana:~/git/assemblage$ describe.ml
+ Loading configure.ml.

==> assemblage cc5a19

└─┬─ lib-assemblage
  ├─── [cmdliner cmdliner ocamlgraph compiler-libs.toplevel opam compiler-libs.toplevel optcomp]
  ├─ flags.ml         flags.mli
  ├─ shell.ml         shell.mli
  ├─ feature.ml       feature.mli
  ├─── Set
  ├─ resolver.ml      resolver.mli
  ├─ git.ml           git.mli
  ├─ action.ml        action.mli
  ├─ build_env.ml     build_env.mli
  ├─ project.ml       project.mli
  ├─── Bin
  ├─── Comp
  ├─── Dep
  ├─── JS
  ├─── Lib
  ├─── Test
  ├─ ocamlfind.ml     ocamlfind.mli
  ├─── META
  ├─ OCaml.ml         OCaml.mli
  ├─ opam.ml          opam.mli
  ├─── Install
  ├─ makefile.ml      makefile.mli
  ├─── Rule
  ├─── Variable
  └─ assemblage.ml    assemblage.mli
└─┬─ bin-configure.ml
  └─ configure.ml
└─┬─ bin-describe.ml
  └─ describe.ml
```

You can find few examples in the `examples/` directory, where projects are built using
multiple local libraries and ocamlfind libraries and syntax extensions.