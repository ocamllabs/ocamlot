#!/bin/sh -x

DIR=`dirname $0`
WORKERS=$1
URL=$2
for i in $(seq 1 $WORKERS); do
  tmux new -d "$DIR/ocamlot_cmd.native work $URL"
done
