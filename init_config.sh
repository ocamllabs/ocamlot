#!/bin/sh
if [ ! -e ./lib/config.ml ]; then
  cp ./lib/config.ml.in ./lib/config.ml
fi
mkdir -p store/public
mkdir -p store/private
