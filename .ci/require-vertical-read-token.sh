#!/usr/bin/env bash
set -euo pipefail

if [[ -n "${VERTICAL_READ_TOKEN:-}" ]]; then
  echo "VERTICAL_READ_TOKEN is present."
  exit 0
fi

if [[ "${GITHUB_EVENT_NAME:-}" == pull_request ]]; then
  echo "Missing required Actions secret VERTICAL_READ_TOKEN. GitHub does not expose repository secrets to fork pull_request workflows, so this run fails closed. A maintainer must run the check from a trusted context with the secret available." >&2
else
  echo "Missing required Actions secret VERTICAL_READ_TOKEN. Configure a fine-grained token with read-only Contents access to luciu5/vertical as a repository Actions secret." >&2
fi
exit 1
