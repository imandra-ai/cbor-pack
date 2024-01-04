#!/bin/sh
DUNE_OPTS="--profile=release --display=quiet"
exec dune exec $DUNE_OPTS -- src/bin/main.exe $@
