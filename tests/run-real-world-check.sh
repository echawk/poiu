#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

export XDG_CACHE_HOME="$REPO_ROOT/.cache"
mkdir -p "$XDG_CACHE_HOME"

if [ "$#" -eq 0 ]; then
  set -- ironclad str osicat alexandria
fi

status=0
for system in "$@"; do
  echo "==> $system"
  if ! POIU_TEST_SYSTEM="$system" sbcl --noinform --non-interactive --no-userinit --no-sysinit --load "$SCRIPT_DIR/load-real-world-system.lisp"; then
    status=1
  fi
done

exit "$status"
