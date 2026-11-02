#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 5 ]]; then
  echo "Usage: run-private-vertical-command.sh SHA PRIVATE_LOG OPERATION -- COMMAND [ARG ...]" >&2
  exit 2
fi

selected_sha="$1"
private_log="$2"
operation="$3"
shift 3
if [[ "$1" != -- ]]; then
  echo "Expected -- before the private command." >&2
  exit 2
fi
shift
if [[ ! "$selected_sha" =~ ^[[:xdigit:]]{40}$ || $# -eq 0 ]]; then
  echo "A full selected vertical SHA and private command are required." >&2
  exit 2
fi

mkdir -p "$(dirname "$private_log")"
if "$@" >"$private_log" 2>&1; then
  echo "Private vertical $operation passed at SHA $selected_sha."
else
  status=$?
  echo "Private vertical $operation failed at SHA $selected_sha. Diagnostics remain on the runner and were not published; reproduce the failure in the private vertical repository." >&2
  exit "$status"
fi
