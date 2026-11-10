# Install and check the iovrse R packages

For ordinary use, source the public helper and request the whole cohort or a
package plus its dependencies:

```r
source("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R")
install_iovrse()                         # all seven packages
# install_iovrse(packages = "vertical") # a smaller deterministic workflow
```

The helper installs required R-package dependencies from public repositories,
then installs the maintained iovrse packages in dependency order. It preserves
already compatible versions and does not let historical `Remotes` pins
replace a newer core. It checks the actual minimum versions; an unavailable
compatible release is an error, never a reason to substitute an older core.
An ordinary installation needs no Git checkout, release SHA, Slurm account,
personal research library, Pandoc or TeX.

CmdStanR's **R package** is included when antitrustBayes is selected. CmdStan
itself and a C++ toolchain are needed only for operations that compile or fit
Stan models. The helper does not download or build CmdStan. Follow the Bayesian
package's separate estimation setup before using those operations.

Use `lib = "path/to/your/R/library"` to select a writable library explicitly.
Compatible packages are kept; an older loaded namespace must be updated in a
fresh R session. `iovrse_install_plan()` prints the current version plan without
installing anything or creating a library.

## Local development and CI

The remote recipe requires the compatible cohort and this helper to have been
published. Uncommitted local fixes are not available through public GitHub
URLs. Before publishing a coordinated change, validate its sibling checkouts:

```r
source("tools/install_iovrse.R")
install_iovrse(source_root = "path/to/parent/of/seven/checkouts",
               lib = "path/to/designated/development/library")
```

This optional route installs the supplied sources directly without creating
source snapshots or worktrees. It refreshes equal-version local sources so
development changes are actually exercised, and refuses to downgrade a newer
installed version. The ordinary remote path needs no sibling checkouts.

`tests/test-install-iovrse.R` checks dependency closure, protection against
recursive pinned downgrades and honest failure reporting without doing a real
installation. `.ci/smoke_iovrse.R` executes the documented deterministic examples
using installed packages from outside the checkout and constructs the GUI
without starting a browser/server. The ordinary-use workflow runs this in
clean Windows, macOS and Linux libraries. Full inference and release-source
certification remain separate; adding the workflow does not claim those
hosted jobs have already passed.
