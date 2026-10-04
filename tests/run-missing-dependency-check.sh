#!/bin/sh
# Check that POIU builds a system with missing dependencies between its files,
# in both deterministic and non-deterministic modes.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

export XDG_CACHE_HOME="$REPO_ROOT/.cache/missing-dependency"
mkdir -p "$XDG_CACHE_HOME"

for deterministic in 1 0; do
  POIU_DETERMINISTIC=$deterministic sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load "$SCRIPT_DIR/run-missing-dependency-check.lisp"
done
