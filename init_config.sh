#!/bin/sh
if [ ! -e ./lib/config.ml ]; then
  cp ./lib/config.ml.in ./lib/config.ml
fi
if [ ! -e ./state ]; then
  git clone git@github.com:ocamlot/ocamlot-state.git state
  cd state
  git config --local --add user.name ocamlot
  git config --local --add user.email infrastructure@lists.ocaml.org
fi
