## Assemblage

> __Assemblage__ is an artistic process. In the visual arts, it
  consists of making three-dimensional or two-dimensional artistic
  compositions by putting together found objects.
  [wikipedia](http://en.wikipedia.org/wiki/Assemblage_(art))

The *Assemblage* toolbox provides an API and a set of binaries to
setup, manage, and use OCaml projects.

### Status

[![Build Status](https://travis-ci.org/samoht/assemblage.svg?branch=master)](https://travis-ci.org/samoht/assemblage)

The library can be used to describe OCaml projects with static dependencies.

The latest public documentation is available [here](http://samoht.github.io/assemblage/Assemblage.html).

A projects description consists of a list of libraries, binaries
and/or toplevels.  Each containing a list of compilation units with
precise (and static) dependency relationships.

Using a project description, the tools can generate:

- a `.merlin` to help edit the project;
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

Every project is parametrized by a set of flags, which will determine
sub-parts of the project to be built (and installed) or not. The flag
can be enabled or disabled either programmatically (ie. in the project
description itself) or passed on the command-line using an integrated
`Cmdliner` interface.

### Examples

The interaction with the system occurs through the `assemblage` tool
which has the `setup` and `describe` sub-commands. `setup`
reads a project description (usually, an `assemble.ml` file located at
the root of the project) and generates the boilerplate files to build,
install and use the project. `describe` simply displays a summary
of the project:

```shell
thomas@piana:~/git/assemblage$ assemblage describe
==> Loading assemble.ml

==> assemblage 3b27e2
└─┬─ lib-assemblage
  ├─── [cmdliner]
  ├─ as_shell.ml      as_shell.mli
  ├─ as_git.ml        as_git.mli
  ├─ as_makefile.ml   as_makefile.mli
  ├─── Rule
  ├─── Var
  ├─ as_features.ml   as_features.mli
  ├─── Set
  ├─ as_flags.ml      as_flags.mli
  ├─── PhaseSet
  ├─ as_resolver.ml   as_resolver.mli
  ├─ as_build_env.ml  as_build_env.mli
  ├─ as_action.ml     as_action.mli
  ├─── FileSet
  ├─ as_component.ml  as_component.mli
  ├─── Bin
  ├─── Container
  ├─── Doc
  ├─── Lib
  ├─── Other
  ├─── Pkg
  ├─── Rule
  ├─── Set
  ├─── Test
  ├─── Unit
  ├─ as_project.ml    as_project.mli
  ├─ as_opam.ml       as_opam.mli
  ├─── Install
  ├─ as_merlin.ml     as_merlin.mli
  ├─── Directive
  ├─ as_ocamlfind.ml  as_ocamlfind.mli
  ├─── META
  ├─ as_project_makefile.mlas_project_makefile.mli
  ├─ as_OCaml_incl.ml
  ├─── Pparse
  ├─── StringSet
  ├─ as_OCaml.ml      as_OCaml.mli
  ├─ as_env.ml        as_env.mli
  ├─ as_tool.ml       as_tool.mli
  ├─ as_cmd.ml        as_cmd.mli
  ├─ assemblage.ml    assemblage.mli
  ├─── Action
  ├─── Build_env
  ├─── Features
  ├─── Flags
  └─── Resolver
└─┬─ bin-assemblage
  └─ tool.ml
└─┬─ bin-ctypes-gen
  └─ ctypes_gen.ml
└─┬─ bin-assemble
  └─ assemble.ml
```

You can find few examples in the `examples/` directory, where projects
are built using multiple local libraries and ocamlfind libraries and
syntax extensions.
