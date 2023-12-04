#!/bin/sh

OPTS="--display=quiet --profile=release"
exec dune exec $OPTS benchs/basic1/run.exe -- $@
