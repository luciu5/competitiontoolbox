# Dependency stack CI setup

For local application verification and the remaining browser gate, see
[release readiness](READINESS.md). Local checkout fixtures do not replace a
hosted dependency-stack check or a real browser run.

All four upstream repositories, including `luciu5/vertical`, are public. The
workflow clones them over public HTTPS. No personal access token or repository
Actions secret is required, including for fork `pull_request` events. An old
`VERTICAL_READ_TOKEN` secret can be removed; the workflow does not read it.

Each selected ref is resolved to a commit and checked out at that exact SHA
under the runner temporary directory. The resolved commits, installed versions
and test tier are recorded in provenance. Release certification still requires
four explicit full candidate SHAs matching the DESCRIPTION pins; public access
does not weaken commit verification or skip vertical integration tests.

Vertical installation, build and check diagnostics use the same public logs as
the other dependencies. Uploaded artifacts contain provenance and logs only,
not dependency source trees, libraries or package builds.

Run the checkout regression fixtures without network access:

```sh
bash .ci/test-public-dependency-checkout.sh
```
