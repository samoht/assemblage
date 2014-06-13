### OCaml-project

An API to manage OCaml projects.

### Status

This is an experimental project to play with few ideas. Patches and ideas are welcome.

Currently, the library can be used to describe a simple static project,
and generate the corresponding Makefile to build the project.

Projects are defined as a list of libraries, each containing a list of compilation
units. Each compilation units have a unique set of (static) dependencies. Three kinds
of dependencies are currently supported:

- local libraries, defined in the same project
- ocamlfind libraries
- ocamlfind syntax extensions, using `camlp4o`

### How to use it

You can find few examples in the `examples/` directory, where projects are built using
multiple local libraries and ocamlfind libraries and syntax extensions. To generate a
Makefile for a given project, simply run `ocaml make.ml`.