# OCamlot : OCaml Online Testing

OCamlot (OCaml Online Testing), is a Continuous Integration and Testing
system for packages provided via OPAM. It comprises all the appropriate
tools and libraries to enable management and development of the machine
infrastructure, for example an OCaml library to interface with Github
(available through OPAM). If you submit a package via OPAM, you will be
rewarded with regular regression tests across many diverse operating systems
and build tool environments. Ultimately, we would also like to integrate
benchmarking, constraint exploration, experimental design, complete
isolation, and domain testing into this system.


## Usage

### Server

 - `./ocamlot_cmd.native serve`

### Worker

 - `../ocamlot/install_ocaml.sh <NICKNAME> <PATH_TO_OCAML_SRC>`
 - `../ocamlot/ocamlot_cmd.native work <URL>`

## Requirements

System libraries:

 - libssl

Pinned Dev packages:

 - avsm/ocaml-github@master
 - avsm/ocaml-cohttp@master
 - mirage/ocaml-cow@master

These packages are installable with `./setup_deps.sh`

OPAM Packages:

 - oasis-mirage
 - cohttp
 - cryptokit
 - github
 - lwt
 - cmdliner
 - re
 - sexplib
 - uri
 - cow

These packages are installable with `./install_deps.sh`

## Build

 - `./setup_deps.sh`
 - `./install_deps.sh`
 - `oasis setup`
 - `make`
