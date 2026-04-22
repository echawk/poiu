#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
MODES=${POIU_TEST_MODES:-"sequential parallel"}

if [ "$#" -eq 0 ]; then
  set -- ironclad str osicat alexandria split-sequence april petalisp
fi

status=0
for system in "$@"; do
  echo "==> $system"
  for mode in $MODES; do
    cache_root="$REPO_ROOT/.cache/real-world/$system/$mode"
    mkdir -p "$cache_root"
    echo "-- $mode"
    if ! XDG_CACHE_HOME="$cache_root" \
         POIU_PLAN_MODE="$mode" \
         POIU_TEST_SYSTEM="$system" \
         sbcl --noinform --non-interactive --no-userinit --no-sysinit \
         --load "$SCRIPT_DIR/load-real-world-system.lisp"; then
      status=1
    fi
  done
done

exit "$status"
