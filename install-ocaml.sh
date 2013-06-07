#!/bin/sh

OCAML_PREFIX_DIR=`pwd`/ocaml
OCAML_SLUG=$1
OCAML_SRC_DIR=$2

mkdir -p $OCAML_PREFIX_DIR

cd $OCAML_SRC_DIR
./configure -prefix $OCAML_PREFIX_DIR/$OCAML_SLUG
make world.opt
make install
