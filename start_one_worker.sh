#!/bin/sh -x

DIR=`dirname $0`
URL=$1

if [ "$URL" = "" ]; then
  URL="https://ocaml-www3.ocamllabs.cl.cam.ac.uk"
fi

mkdir -p log
export OPAMJOBS=1
while true; do
   $DIR/ocamlot_cmd.native work $URL
done
