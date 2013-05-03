# OCamlot : OCaml Online Testing

## Usage

This early version of ocamlot is intended to be used solely as
command-line driven automation while the service infrastructure is being
written. OCamlot expects to operate in the root of its checked out
repository where it has been built with `make` and it will store its
working copies under the subdirectory `work`. You will need to edit
`setup.data` to add `-X work` under ocamlbuildflags to suppress the
hygiene warning. You will also need to use the ocaml-github Github auth
token cookie jar with a local token named 'ocamlot' to grant Github
access.

## Requirements

avsm/ocaml-cohttp@master
dsheets/ocaml-github@request-control
OCamlPro/opam@master
