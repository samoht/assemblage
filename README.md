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

The latest public documentation is available [here](http://samoht.github.io/assemblage/Assemblage.html).

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

### Examples

There are currently two tools installed by *Assemblage*: `configure.ml` and
`describe.ml`. `configure.ml` reads a project description (usually,
an `assemble.ml` file located at the root of the project) and generates the
boilerplate files to build, install and use the project. `describe.ml` simply
displays a summary of the project:

```shell
$ thomas@piana:~/git/assemblage$ describe.ml

==> assemblage 527f65

└─┬─ lib-assemblage
  ├─── [cmdliner compiler-libs.toplevel ocamlgraph optcomp]
  ├─ as_flags.ml      as_flags.mli
  ├─ as_features.ml   as_features.mli
  ├─── Set
  ├─ as_shell.ml      as_shell.mli
  ├─ as_resolver.ml   as_resolver.mli
  ├─ as_build_env.ml  as_build_env.mli
  ├─ as_git.ml        as_git.mli
  ├─ as_action.ml     as_action.mli
  ├─ as_project.ml    as_project.mli
  ├─── Bin
  ├─── C
  ├─── CU
  ├─── Component
  ├─── Component.Graph
  ├─── Component.Set
  ├─── Gen
  ├─── JS
  ├─── Lib
  ├─── Pkg
  ├─── Test
  ├─ as_ocamlfind.ml  as_ocamlfind.mli
  ├─── META
  ├─ as_makefile.ml   as_makefile.mli
  ├─── Rule
  ├─── Variable
  ├─ as_OCaml.ml      as_OCaml.mli
  ├─ as_opam.ml       as_opam.mli
  ├─── Install
  ├─ assemblage.ml    assemblage.mli
  ├─── Action
  ├─── Build_env
  ├─── Features
  ├─── Flags
  └─── Resolver
└─┬─ bin-configure.ml
  └─ configure.ml
└─┬─ bin-describe.ml
  └─ describe.ml
└─┬─ bin-ctypes-gen
  └─ ctypes_gen.ml
```

You can find few examples in the `examples/` directory, where projects are built using
multiple local libraries and ocamlfind libraries and syntax extensions.
