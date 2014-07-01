## Assemblage



> __Assemblage__ is an artistic process. In the Programming Language
   arts, it consists of making complex project compositions by putting
   together build artefacts.

The *Assemblage* toolbox provides an API and a set of binaries to
configure, manage, and use OCaml projects.

### Status

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

You can find few examples in the `examples/` directory, where projects are built using
multiple local libraries and ocamlfind libraries and syntax extensions.