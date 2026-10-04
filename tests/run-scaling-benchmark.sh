#!/bin/sh
# Benchmark POIU against sequential ASDF on synthetic systems of various shapes.
# Usage: sh tests/run-scaling-benchmark.sh [shape ...]   (shapes: wide layered chain tiny large)
set -eu
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
POIU_SCALING_SHAPES="$*" exec sbcl --noinform --non-interactive --no-userinit --no-sysinit \
  --load "$SCRIPT_DIR/scaling.lisp"
