#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: resolve-private-vertical-checkout.sh REQUESTED_REF CHECKOUT_DIR" >&2
  exit 2
fi

requested_ref="$1"
checkout_dir="$2"
if ! commit="$(git -C "$checkout_dir" rev-parse --verify 'HEAD^{commit}' 2>/dev/null)"; then
  echo "The private vertical checkout has no resolvable HEAD commit." >&2
  exit 1
fi

if [[ "$requested_ref" =~ ^[[:xdigit:]]{40}$ && "${commit,,}" != "${requested_ref,,}" ]]; then
  echo "The private vertical checkout resolved to $commit, which does not match the selected SHA." >&2
  exit 1
fi

printf '%s' "$commit"
