# Dependency stack CI setup

The dependency stack workflow checks out `luciu5/vertical`, which is a private
repository. Configure a repository Actions secret named `VERTICAL_READ_TOKEN`
with a fine-grained personal access token limited to the `luciu5/vertical`
repository and read-only **Contents** access. The workflow uses that secret
only for its missing-secret guard and the dedicated checkout action; checkout
credentials are not persisted in Git configuration.

Create the token in GitHub with only that repository selected, then enter it at
the GitHub CLI's interactive prompt:

```sh
gh secret set VERTICAL_READ_TOKEN --repo github.com/luciu5/competitiontoolbox
```

The value belongs in the CLI prompt, not in a shell argument or a chat message.

GitHub does not provide repository secrets to workflows triggered by fork
`pull_request` events. Those runs fail closed with an explanatory message. A
maintainer can run the workflow from a trusted branch or use workflow dispatch
after reviewing the proposed change. Manual dispatch requires the workflow on
the default branch; until then, use a trusted release-branch push. The workflow never switches to
`pull_request_target` to obtain the secret.

The private checkout is pinned to `VERTICAL_REF`, verified against that ref
when it is a full SHA, moved under the runner temporary directory, and recorded
in the same provenance file as the three public source resolutions. Public
artifacts contain provenance and logs only.
