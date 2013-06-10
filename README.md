# OCamlot : OCaml Online Testing

## Usage

### Server

./ocamlot_cmd.native serve

### Worker

../ocamlot/install_ocaml.sh <NICKNAME> <PATH_TO_OCAML_SRC>
../ocamlot/ocamlot_cmd.native work <URL>

## Requirements

System libraries:

libssl

OPAM Packages:

oasis-mirage, cohttp, cryptokit, github, lwt, cmdliner, re, sexplib, uri

These packages are installable with `./install_deps.sh`

Pinned Dev packages:

avsm/ocaml-github@master

dsheets/ocaml-cohttp@magic-biscuit

## Build

oasis setup
make
