#!/usr/bin/env Rscript
# Maintainer-only: install the source cohort explicitly before invoking this.
# This script never installs dependencies or packages and never publishes.
args <- commandArgs(trailingOnly = TRUE)
source_root <- normalizePath(if (length(args)) args[1] else "..", mustWork = TRUE)
do_check <- "--check" %in% args
packages <- c("antitrust", "coordination", "trade", "vertical", "financial",
              "antitrustBayes", "competitiontoolbox")
log_root <- file.path(source_root, "competitiontoolbox", ".scratch", "documentation")
dir.create(log_root, recursive = TRUE, showWarnings = FALSE)
for (pkg in c(packages, "pkgdown", "knitr", "rmarkdown")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing documentation dependency: ", pkg)
}
if (!rmarkdown::pandoc_available()) stop("Pandoc is required only for this maintainer build")
provenance <- file.path(log_root, "provenance.txt")
writeLines(c(format(Sys.time(), tz = "UTC"), capture.output(sessionInfo())), provenance)
for (pkg in packages) {
  source <- file.path(source_root, pkg)
  dcf <- read.dcf(file.path(source, "DESCRIPTION"))
  if (packageVersion(pkg) != package_version(dcf[1, "Version"]))
    stop("Install the current source cohort before rendering: ", pkg)
  installed <- find.package(pkg)
  git <- function(arguments) system2("git", c("-C", shQuote(source), arguments), stdout = TRUE)
  files <- file.path(source, git("ls-files --cached --others --exclude-standard"))
  files <- unique(files[file.exists(files) & !dir.exists(files)])
  hashes <- tools::md5sum(files)
  names(hashes) <- substring(names(hashes), nchar(source) + 2L)
  saveRDS(hashes, file.path(log_root, paste0(pkg, "-source-md5.rds")))
  installed_files <- list.files(installed, recursive = TRUE, full.names = TRUE)
  installed_hashes <- tools::md5sum(installed_files[!dir.exists(installed_files)])
  names(installed_hashes) <- substring(names(installed_hashes), nchar(installed) + 2L)
  saveRDS(installed_hashes, file.path(log_root, paste0(pkg, "-installed-md5.rds")))
  cat("\n", pkg, " ", as.character(packageVersion(pkg)), " loaded from ", installed,
      "\nHEAD: ", paste(git("rev-parse HEAD"), collapse = "\n"),
      "\nDirty state (source hashes saved separately):\n",
      paste(git("status --short"), collapse = "\n"), "\n", file = provenance, append = TRUE, sep = "")
  # Rd macros in some packages read DESCRIPTION relative to the working directory.
  old <- setwd(source)
  pkgdown::build_site(pkg = source, devel = FALSE, install = FALSE,
    new_process = TRUE, preview = FALSE, run_dont_run = FALSE,
    override = list(destination = ".scratch/pkgdown"))
  setwd(old)
  build_dir <- file.path(log_root, pkg)
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  old <- setwd(build_dir)
  status <- system2(file.path(R.home("bin"), "R"), c("CMD", "build", shQuote(source)),
                    stdout = "build.log", stderr = "build.log")
  if (status != 0) stop("Vignette source-package build failed: ", pkg, "; see ", build_dir)
  tarball <- file.path(build_dir, paste0(pkg, "_", dcf[1, "Version"], ".tar.gz"))
  if (!file.exists(tarball)) stop("Expected current source bundle in ", build_dir)
  if (do_check) {
    # Economic suites run in existing QA jobs. This checks documentation,
    # package structure, examples, and installed vignette discovery.
    status <- system2(file.path(R.home("bin"), "R"),
      c("CMD", "check", "--no-manual", "--no-tests", shQuote(tarball)),
      stdout = "check.log", stderr = "check.log")
    if (status != 0) stop("Documentation/package check failed: ", pkg, "; see ", build_dir)
  }
  setwd(old)
}
cat("Documentation review artifacts: ", log_root, "\n", sep = "")
