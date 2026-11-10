# Ordinary installed-package smoke checks. No backend inference or GUI launch.
args <- commandArgs(TRUE)
source_root <- normalizePath(if (length(args)) args[[1L]] else "..", mustWork = TRUE)
packages <- c("antitrust", "coordination", "trade", "vertical", "financial",
              "antitrustBayes", "competitiontoolbox")
versions <- vapply(packages, function(package) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Missing installed package: ", package)
  as.character(utils::packageVersion(package))
}, character(1))
print(data.frame(Package = packages, Version = versions), row.names = FALSE)

read_blocks <- function(path) {
  lines <- readLines(path, warn = FALSE)
  blocks <- list(); active <- FALSE; code <- character()
  for (line in lines) {
    if (!active && grepl("^```[rR][[:space:]]*$", line)) {
      active <- TRUE; code <- character()
    } else if (active && grepl("^```[[:space:]]*$", line)) {
      blocks[[length(blocks) + 1L]] <- paste(code, collapse = "\n")
      active <- FALSE
    } else if (active) code <- c(code, line)
  }
  blocks
}

# Ordinary examples should work without a checkout working directory or CmdStan.
setwd(tempdir())
Sys.setenv(CMDSTAN = file.path(tempdir(), "cmdstan-not-needed-for-these-examples"))
options(mc.cores = 1L)
for (package in setdiff(packages, "competitiontoolbox")) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
  blocks <- read_blocks(file.path(source_root, package, "README.md"))
  if (package == "antitrustBayes") {
    blocks <- Filter(function(code) grepl("bayes_specify\\(", code) &&
      !grepl("bayes_est\\(", code), blocks)
  } else {
    blocks <- Filter(function(code) !grepl(
      "install_iovrse|install[.]packages|install_github|trade_shiny|ct_shiny", code), blocks)
  }
  if (!length(blocks)) stop("No self-contained ordinary README example: ", package)
  example_env <- new.env(parent = globalenv())
  for (i in seq_along(blocks)) {
    if (grepl("bayes_est|install_cmdstan|bridge_sampler|cmdstan_model|rstan::", blocks[[i]]))
      stop("Inference/setup belongs in the extended tier, not this smoke: ", package)
    set.seed(701L + i)
    eval(parse(text = blocks[[i]]), envir = example_env)
  }
  cat("PASS:", package, length(blocks), "installed-package README examples\n")
}

# Construct the current app without opening a browser or starting a server.
app_dir <- system.file("ct_shiny", package = "competitiontoolbox")
if (!nzchar(app_dir)) stop("Installed GUI application files are missing.")
app <- shiny::shinyAppDir(app_dir)
stopifnot(inherits(app, "shiny.appobj"))
cat("PASS: competitiontoolbox application construction (no server/browser launched)\n")
