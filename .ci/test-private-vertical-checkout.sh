#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workflow="$repo_root/.github/workflows/dependency-stack-qa.yml"
guard="$repo_root/.ci/require-vertical-read-token.sh"
resolve="$repo_root/.ci/resolve-private-vertical-checkout.sh"
private_command="$repo_root/.ci/run-private-vertical-command.sh"

# This infrastructure regression test catches a material cross-package CI
# failure: certification could otherwise test a different private vertical
# commit than the SHA recorded beside the public dependency source pins.
python3 - "$workflow" "$repo_root/.ci/README.md" <<'PY'
from pathlib import Path
import re
import sys

workflow = Path(sys.argv[1]).read_text()
readme = Path(sys.argv[2]).read_text()

assert "pull_request_target" not in workflow, "private source access must not use pull_request_target"
assert workflow.count("${{ secrets.VERTICAL_READ_TOKEN }}") == 2, (
    "the token must appear only in the guard-step environment and checkout token input"
)

checkouts = re.findall(r"uses: actions/checkout@([0-9a-f]{40})", workflow)
assert len(checkouts) == 2 and checkouts[0] == checkouts[1], (
    "vertical checkout must reuse the repository's pinned actions/checkout release"
)

guard_start = workflow.index("      - name: Require private vertical read token")
checkout_start = workflow.index("      - name: Checkout selected vertical source")
move_start = workflow.index("      - name: Move and verify vertical source checkout")
resolve_start = workflow.index("      - name: Resolve dependency source refs")
assert guard_start < checkout_start < move_start < resolve_start, (
    "secret guard, dedicated checkout, and relocation must precede dependency installation"
)

checkout_block = workflow[checkout_start:move_start]
for required in (
    "repository: luciu5/vertical",
    "ref: ${{ env.VERTICAL_REF }}",
    "token: ${{ secrets.VERTICAL_READ_TOKEN }}",
    "persist-credentials: false",
    "fetch-depth: 1",
):
    assert required in checkout_block, f"vertical checkout is missing {required!r}"

assert 'clone_at_ref luciu5/antitrust ' in workflow
assert 'clone_at_ref luciu5/coordination ' in workflow
assert 'clone_at_ref luciu5/trade ' in workflow
assert 'clone_at_ref luciu5/vertical ' not in workflow, (
    "the private package must not regress to a plain HTTPS clone"
)
assert 'mv "$checkout_path" "$SOURCE_ROOT/vertical"' in workflow, (
    "the private source and its Git directory must move out of GITHUB_WORKSPACE"
)
assert "resolve-private-vertical-checkout.sh" in workflow
assert 'PRIVATE_LOG_ROOT="$ci_root/private-logs"' in workflow
assert workflow.count("run-private-vertical-command.sh") >= 3, (
    "private install, source build, and package check output must use the redacting runner-local wrapper"
)
assert 'tee "$LOG_ROOT/vertical-' not in workflow, (
    "private vertical diagnostics must not enter the uploaded public logs"
)

artifact_start = workflow.index("      - name: Upload provenance and CI logs")
artifact_block = workflow[artifact_start:]
assert "provenance.txt" in artifact_block and "/logs" in artifact_block
assert all(path not in artifact_block for path in ("/src", "/build", "private-logs")), (
    "public artifacts must not include source checkouts, libraries, builds, or private logs"
)

assert readme.count("VERTICAL_READ_TOKEN") >= 1
assert "read-only **Contents** access" in readme
assert "GitHub does not provide repository secrets" in readme
assert "`pull_request` events" in readme

pandoc = workflow.index("Set up Pandoc for R Markdown vignettes")
extended = workflow.index("Run EXTENDED parity and upstream regression checks")
tinytex = workflow.index("Set up TinyTeX for --as-cran manuals")
nightly = workflow.index("Run NIGHTLY full toolbox check")
assert pandoc < extended and tinytex < extended and tinytex < nightly, (
    "nightly upstream vignette/manual prerequisites must be installed before checks"
)
print("private vertical workflow structure: ok")
PY

# The guard must fail with an actionable fork-PR message while reporting only
# secret presence. This fixture uses a dummy value, never an actual credential.
if output="$(env -u VERTICAL_READ_TOKEN GITHUB_EVENT_NAME=pull_request bash "$guard" 2>&1)"; then
  echo "token guard unexpectedly accepted a missing secret" >&2
  exit 1
fi
[[ "$output" == *"VERTICAL_READ_TOKEN"* && "$output" == *"fork pull_request"* ]]

dummy_token='fixture-only-not-a-credential'
output="$(VERTICAL_READ_TOKEN="$dummy_token" GITHUB_EVENT_NAME=push bash "$guard" 2>&1)"
[[ "$output" == *"VERTICAL_READ_TOKEN is present"* ]]
[[ "$output" != *"$dummy_token"* ]]

# Exercise selected-SHA acceptance, branch resolution, and candidate mismatch
# using a throwaway local Git fixture; no network or real repository is used.
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT
git -C "$fixture_root" init -q -b main
git -C "$fixture_root" -c user.name='CI fixture' -c user.email='ci-fixture@example.invalid' \
  commit --allow-empty -q -m 'fixture commit'
selected_sha="$(git -C "$fixture_root" rev-parse HEAD)"
resolved="$(bash "$resolve" "$selected_sha" "$fixture_root")"
[[ "$resolved" == "$selected_sha" ]]
resolved_branch="$(bash "$resolve" main "$fixture_root")"
[[ "$resolved_branch" == "$selected_sha" ]]

# Verify that failed private package output stays on the runner only. The
# marker models a test/source excerpt that would disclose private diagnostics.
if output="$(bash "$private_command" "$selected_sha" \
  "$fixture_root/private-diagnostics.log" 'fixture check' -- \
  bash -c 'echo PRIVATE_DIAGNOSTIC_MARKER >&2; exit 7' 2>&1)"; then
  echo "private command wrapper unexpectedly accepted a failing command" >&2
  exit 1
fi
[[ "$output" == *"reproduce the failure in the private vertical repository"* ]]
[[ "$output" == *"$selected_sha"* ]]
[[ "$output" != *"PRIVATE_DIAGNOSTIC_MARKER"* ]]
private_diagnostics="$(cat "$fixture_root/private-diagnostics.log")"
[[ "$private_diagnostics" == *"PRIVATE_DIAGNOSTIC_MARKER"* ]]

wrong_sha='0000000000000000000000000000000000000000'
if output="$(bash "$resolve" "$wrong_sha" "$fixture_root" 2>&1)"; then
  echo "vertical SHA verifier unexpectedly accepted a mismatched candidate" >&2
  exit 1
fi
[[ "$output" == *"does not match the selected SHA"* ]]

echo "private vertical checkout fixtures: ok"
