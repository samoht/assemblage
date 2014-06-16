### OCaml-tools

A set of tools to manage the configuration of OCaml projects.

### Status

The library can be used to describe OCaml projects with static dependencies,
generate the corresponding `Makefile` to build the project, and generate the
correponding `META` and `<pkg>.install` files to install it.

A projects description consists of a list of libraries, binaries and/or toplevels.
Each containing a list of compilation units with precise (and static) dependency
relationships.

#### Dependency Tracking

Multiple kinds of dependencies are currently supported:

- local libraries, defined in the same project
- local syntax extensions, defined in the same project (using `camlp4o`)
- global ocamlfind libraries
- global ocamlfind syntax extensions (using `camlp4o`)

#### Project Flags

Every project is parametrized by a set of flags, which will determine sub-parts
of the project to be built (and installed) or not. The flag can be enabled or
disabled either programmatically (ie. in the project description itself) or
passed on the command-line using an integrated `Cmdliner` interface.

#### `opam-configure`

The package also installs a `opam-configure` helper scripts, which can be used
to configure an OCaml project using the state of OPAM in the user computer.

### Examples

You can find few examples in the `examples/` directory, where projects are built using
multiple local libraries and ocamlfind libraries and syntax extensions. To generate a
Makefile for a given project, run `make` at the root of this repository and then
simply run `../../configure.top configure.ml [-h]` at the root of each project examples.
