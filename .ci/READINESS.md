# Local application and hosted release gates

Run development-source tests with an explicitly compatible isolated library:

```sh
R_LIBS=/path/to/compatible/library \
  Rscript --vanilla -e 'testthat::test_local(filter="app-interactions", reporter="summary")'
COMPETITIONTOOLBOX_TEST_TIER=FAST R_LIBS=/path/to/compatible/library \
  Rscript --vanilla -e 'testthat::test_local(reporter="summary")'
bash .ci/test-public-dependency-checkout.sh
```

The application-interaction tests run the actual Shiny server, exercise all
four pages, show an invalid-price error and recover, and execute copied R code
for edited scenarios in independent R processes. These protect reactive wiring
and standalone reproducibility that registry/construction checks miss. They
provide no browser evidence. Warnings remain visible; finite solver output
alone does not certify an equilibrium. The core and extended economic tests
provide the separate numerical/economic checks.

Reference panels use antitrust's current `Workflow` vignette and trade's
`Reference`. Installed HTML is served through the application's resource path,
so a remote visitor does not receive a server-local dynamic-help address.
Development installs without vignettes show an explicit unavailable message and
a link to the current public online guide; reading help does not require building
vignettes locally. The navbar mounts these server panels.

A socket-enabled browser run remains required before a public release:

1. Launch the installed frozen app and record package/source provenance.
2. Navigate Horizontal, Vertical, Tariffs and Quotas; edit an input and run each
   scenario. Inspect summary, details, elasticities, diagnostics and R Code.
3. Change a model and rerun; check that displayed results follow the selected
   inputs. Trigger a deliberate invalid input, inspect the visible message,
   restore valid data and rerun.
4. Inspect numerical-simulation plots and installed reference links. Execute
   copied R code in a fresh R session and compare the selected scenario's
   results. Retain browser-console/app errors and screenshots.

There is currently no user-facing download handler/button/link in the app.
The export feature is copied R code; a “download smoke” is inapplicable until
such a feature exists. Do not add a new download feature merely to clear a
historical checklist item.

The former private-vertical token guard has been removed because vertical is
public. Hosted checks now exercise the full public dependency stack, including
vertical, without repository secrets. Historical waived failures remain
historical evidence; only a completed successful run certifies its recorded
source commits. See [the setup instructions](README.md) for provenance and exact
commit checks. Repository visibility and public package publication remain
separate decisions.

## Documentation walkthrough acceptance

The guide `vignettes/gui-to-r-merger.Rmd` uses the four-row default horizontal
Logit/Bertrand market. For manual browser acceptance, record package versions,
run its explicit controls, inspect Messages/Diagnostics/Details, make the -.04
Prod1 cost edit, and copy the complete R Code into a fresh-session script. Compare
unrounded prices/shares with the app and retain any errors and two readable
screenshots. Also check invalid-price error recovery and the stored-results
captions. Headless script/link tests do not certify this browser step.

Maintainer-only rendering and build evidence: [.ci/DOCUMENTATION.md](DOCUMENTATION.md).
