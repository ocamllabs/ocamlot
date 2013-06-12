#!/bin/sh -x

DIR=`dirname $0`
WORKERS=$1
for i in $(seq 1 $WORKERS); do
  tmux kill-session -t worker$i
done
