#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

export XDG_CACHE_HOME="$REPO_ROOT/.cache"
export POIU_TEST_LOG="$SCRIPT_DIR/concurrency-events.log"
mkdir -p "$XDG_CACHE_HOME"

exec sbcl --noinform --non-interactive --load "$SCRIPT_DIR/run-concurrency-check.lisp"
