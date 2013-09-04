#!/bin/sh -x

DIR=`dirname $0`
URL=$1

if [ "$URL" = "" ]; then
  URL="https://ocamlot.recoil.org"
fi

mkdir -p log
export OPAMJOBS=1
while true; do
   $DIR/ocamlot_cmd.native work $URL
done
