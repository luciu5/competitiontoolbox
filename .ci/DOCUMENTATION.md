# Maintainer documentation build

Ordinary installation deliberately uses `build_vignettes = FALSE` and
`build_manual = FALSE`. Readers do not need Pandoc, TeX, pkgdown or CmdStan.
Sources live in each package's `vignettes/`; the README maps questions to guides.
Stable help IDs `antitrust::Workflow` and `trade::Reference` are retained.

The manual **Documentation review artifacts** Actions workflow checks out the
seven maintained packages and installs that source cohort once in one isolated
job library using the existing installer. Documentation tools are installed
explicitly in that setup step, never in tests, workers, vignettes or the app.
It renders sites, builds source packages including vignettes, and runs package
checks with `--no-manual --no-tests`; economic regression tests remain in existing
QA workflows. No deployment permissions or Pages publication are configured.

To run after explicitly installing a compatible local cohort and doc tools:

```sh
Rscript --vanilla .ci/build_documentation.R /path/to/sibling/checkouts --check
```

Omit `--check` to render and build only. Keep all package versions consistent
with their source DESCRIPTIONs. The script records loaded paths/versions, Git
HEAD, dirty state and actual source-file hashes. A clean SHA alone does not
identify an uncommitted build. The shared paper library must not be refreshed
while another session or job uses it; use a dedicated documentation library.

Generated sites go to each package's `.scratch/pkgdown`, **not `docs/`**: vertical
has authored material there. Logs, source hashes and source bundles go to
`competitiontoolbox/.scratch/documentation`. Keep these out of source commits.
iopolicy is optional developer infrastructure, outside the seven-package
installer and this public-cohort job; its guide can be built separately with
its own explicitly installed source.

Review the rendered pages for wrapping, units, table labels, links and reading
order. Print preview at normal readable type size should generally stay below
10 pages per guide; source word counts are not page measurements. Do not reduce
font size to force a pass. HTML is primary; no PDF/LaTeX dependency is required.
Optional HMC/bridge and interactive GUI chunks are explicitly unevaluated and
must not be reported as validated by a doc build. Lightweight chunks execute.

Sites are local review artifacts until separately approved for publication.
Keep working source links until rendered public sites exist; do not replace
them with guessed Pages URLs. The GUI still tries installed vignettes first
and has a source fallback for ordinary installations without built guides.
`tests/testthat/test-documentation-access.R` checks both branches.

Browser acceptance remains the procedure in [READINESS.md](READINESS.md).
A headless render or Shiny testServer run does not certify manual browser flow.
