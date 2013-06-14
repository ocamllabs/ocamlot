#!/bin/sh -x

DIR=`dirname $0`
WORKERS=$1
URL=$2
mkdir -p log
export OPAMJOBS=1
for i in $(seq 1 $WORKERS); do
  tmux new -d -s worker$i \
      "$DIR/ocamlot_cmd.native work $URL 2> log/worker$i.err > log/worker$i.out"
done
