#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

# Permanent infrastructure regression: a public dependency must be testable
# from fork PRs without secrets, while an incorrect or missing source revision
# must fail rather than silently using the default branch.
python3 - "$repo_root/.github/workflows/dependency-stack-qa.yml" "$fixture_root/clone.sh" <<'PY'
from pathlib import Path
import sys
import textwrap

workflow = Path(sys.argv[1]).read_text()
assert "secrets." not in workflow, "public stack CI must not require repository secrets"
assert "pull_request_target" not in workflow
for package, variable in (("antitrust", "ANTITRUST"), ("coordination", "COORDINATION"),
                          ("trade", "TRADE"), ("vertical", "VERTICAL")):
    assert f'clone_at_ref luciu5/{package} "${variable}_REF"' in workflow
    assert f'install_source {package} "${variable}_SRC"' in workflow
    assert f'check_upstream {package} "${variable}_SRC"' in workflow
assert '"$VERTICAL_SHA" != "${VERTICAL_REF,,}"' in workflow
assert 'resolved_vertical_sha=$VERTICAL_SHA' in workflow
assert "DESCRIPTION Remotes do not match the four exact release candidate SHAs" in workflow
assert '"$TRADE_SRC" "$VERTICAL_SRC" "$GITHUB_WORKSPACE"' in workflow
artifact = workflow[workflow.index("      - name: Upload provenance and CI logs"):]
assert "provenance.txt" in artifact and "/logs" in artifact
assert all(path not in artifact for path in ("/src", "/build", "/R-lib"))
start = workflow.index("          clone_at_ref() {")
end = workflow.index('\n\n          ANTITRUST_SHA=', start)
Path(sys.argv[2]).write_text(textwrap.dedent(workflow[start:end]) + "\n")
gate_start = workflow.index('          if [[ "$GITHUB_EVENT_NAME"')
gate_end = workflow.index('          mkdir -p "$R_LIBS_USER"', gate_start)
gate = textwrap.dedent(workflow[gate_start:gate_end])
gate += '\nprintf "%s\\n" "$ANTITRUST_REF" "$COORDINATION_REF" "$TRADE_REF" "$VERTICAL_REF"\n'
fixture = Path(sys.argv[2]).parent
(fixture / "pin-gate.sh").write_text(gate)
pins = [str(i) * 40 for i in range(1, 5)]
packages = ["antitrust", "coordination", "trade", "vertical"]
(fixture / "DESCRIPTION").write_text("Package: fixture\nVersion: 0.0.0\nRemotes: " +
    ", ".join(f"luciu5/{name}@{sha}" for name, sha in zip(packages, pins)) + "\n")
print("Public dependency workflow checks: OK")
PY

# Execute the actual event gate, not a duplicate of its condition. Master,
# scheduled and release checks must never drift to moving upstream refs.
for case in schedule push-master push-release pr-master pr-release-head pr-release-base push-refactor pr-refactor dispatch; do
  event=push ref=refs/heads/refactor head='' base='' expected=moving
  case "$case" in
    schedule) event=schedule; expected=pinned ;;
    push-master) ref=refs/heads/master; expected=pinned ;;
    push-release) ref=refs/heads/release/fixture; expected=pinned ;;
    pr-master) event=pull_request; base=master; expected=pinned ;;
    pr-release-head) event=pull_request; head=release/fixture; expected=pinned ;;
    pr-release-base) event=pull_request; base=release/fixture; expected=pinned ;;
    pr-refactor) event=pull_request; head=contributor; base=refactor ;;
    dispatch) event=workflow_dispatch ;;
  esac
  resolved="$(cd "$fixture_root"; GITHUB_EVENT_NAME="$event" GITHUB_REF="$ref" \
    GITHUB_HEAD_REF="$head" GITHUB_BASE_REF="$base" GITHUB_ENV="$fixture_root/env" \
    ANTITRUST_REF=moving COORDINATION_REF=moving TRADE_REF=moving VERTICAL_REF=moving \
    bash -euo pipefail "$fixture_root/pin-gate.sh")"
  if [[ "$expected" == pinned ]]; then
    expected_refs="$(printf '%040d\n' 0 | tr 0 1; printf '%040d\n' 0 | tr 0 2; \
      printf '%040d\n' 0 | tr 0 3; printf '%040d\n' 0 | tr 0 4)"
  else
    expected_refs=$'moving\nmoving\nmoving\nmoving'
  fi
  [[ "$resolved" == "$expected_refs" ]] || { echo "Incorrect source pin gate for $case" >&2; exit 1; }
done
echo 'Source pin event fixtures: master, scheduled, release, development and manual paths passed.'

source "$fixture_root/clone.sh"
git init -q -b main "$fixture_root/upstream"
git -C "$fixture_root/upstream" -c user.name='CI fixture' -c user.email='ci@example.invalid' \
  commit --allow-empty -q -m 'selected source'
selected_sha="$(git -C "$fixture_root/upstream" rev-parse HEAD)"
git -C "$fixture_root/upstream" tag selected
git -C "$fixture_root/upstream" -c user.name='CI fixture' -c user.email='ci@example.invalid' \
  commit --allow-empty -q -m 'newer default source'
default_sha="$(git -C "$fixture_root/upstream" rev-parse HEAD)"

# Redirect only the fixture URL to a local repository: no network, credentials,
# or user/global Git configuration are needed. Exercise the production function.
export GIT_CONFIG_COUNT=1
export GIT_CONFIG_KEY_0="url.file://$fixture_root/upstream.insteadOf"
export GIT_CONFIG_VALUE_0="https://github.com/fixture/public.git"
unset VERTICAL_READ_TOKEN
export GITHUB_EVENT_NAME=pull_request

for requested in "$selected_sha" selected refs/tags/selected main refs/heads/main; do
  destination="$fixture_root/checkout-${requested//\//-}"
  resolved="$(clone_at_ref fixture/public "$requested" "$destination")"
  expected="$selected_sha"
  if [[ "$requested" == main || "$requested" == refs/heads/main ]]; then
    expected="$default_sha"
  fi
  [[ "$resolved" == "$expected" ]]
  [[ "$(git -C "$destination" rev-parse HEAD)" == "$expected" ]]
  if git -C "$destination" symbolic-ref -q HEAD >/dev/null; then
    echo "Selected source checkout must be detached" >&2
    exit 1
  fi
done

for requested in missing-ref 0000000000000000000000000000000000000000; do
  if output="$(clone_at_ref fixture/public "$requested" "$fixture_root/$requested" 2>&1)"; then
    echo "Checkout unexpectedly accepted missing revision $requested" >&2
    exit 1
  fi
  [[ "$output" == *"Cannot resolve"* ]]
done
echo 'Public checkout fixtures: exact SHA, tag, branch, detached HEAD and invalid ref checks passed without secrets.'
