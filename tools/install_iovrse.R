# Public R-package installer. Sourcing this file only defines functions.
# CmdStan itself, compilers, Git checkouts and research environments are not
# prerequisites for the supplied-state/deterministic examples.
.iovrse_packages <- list(
  antitrust = list(ref = "master", version = "0.99.43.9000", deps = character(),
    cran = c(BB = "0", numDeriv = "0", nleqslv = "0", SQUAREM = "0")),
  coordination = list(ref = "main", version = "0.1.0", deps = "antitrust", cran = character()),
  trade = list(ref = "master", version = "0.9.0", deps = c("antitrust", "coordination"), cran = character()),
  vertical = list(ref = "main", version = "0.3.0", deps = "antitrust", cran = c(MASS = "0")),
  financial = list(ref = "main", version = "0.1.0", deps = "antitrust", cran = c(cubature = "0", dplyr = "0")),
  antitrustBayes = list(ref = "main", version = "0.3.0.9002", deps = c("antitrust", "coordination"),
    cran = c(cmdstanr = "0.9.0", bridgesampling = "1.1.0", digest = "0", posterior = "0", loo = "2.5.0")),
  competitiontoolbox = list(ref = "master", version = "0.9.0", deps = c("antitrust", "trade", "vertical"),
    cran = c(shiny = "0", rhandsontable = "0", bslib = "0", ggplot2 = "0")))

.iovrse_order <- function(packages) {
  if (!is.character(packages) || !length(packages) || anyNA(packages) ||
      any(!packages %in% names(.iovrse_packages)))
    stop("Choose package names from: ", paste(names(.iovrse_packages), collapse = ", "), call. = FALSE)
  output <- character()
  visit <- function(package) {
    if (package %in% output) return(invisible(NULL))
    for (dependency in .iovrse_packages[[package]]$deps) visit(dependency)
    output <<- c(output, package)
  }
  for (package in packages) visit(package)
  output
}

.iovrse_version <- function(package, lib) {
  path <- find.package(package, lib.loc = lib, quiet = TRUE)
  if (!length(path)) return(NA_character_)
  unname(read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")[[1L]])
}

.iovrse_satisfies <- function(version, minimum) {
  length(version) == 1L && !is.na(version) &&
    utils::compareVersion(version, minimum) >= 0L
}

.iovrse_path <- function(path) {
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") tolower(path) else path
}

.iovrse_library <- function(lib, create = TRUE) {
  if (is.null(lib)) {
    writable <- .libPaths()[file.access(.libPaths(), 2L) == 0L]
    if (length(writable)) lib <- writable[[1L]] else {
      user <- Sys.getenv("R_LIBS_USER", unset = "")
      if (!nzchar(user)) stop("Choose a writable R library with lib=PATH.", call. = FALSE)
      lib <- strsplit(user, .Platform$path.sep, fixed = TRUE)[[1L]][[1L]]
    }
  }
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) || !nzchar(lib))
    stop("'lib' must name one writable R library.", call. = FALSE)
  lib <- path.expand(lib)
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", lib)) lib <- file.path(getwd(), lib)
  if (create) dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  parent <- lib
  while (!dir.exists(parent) && dirname(parent) != parent) parent <- dirname(parent)
  if (!dir.exists(parent) || file.access(parent, 2L) != 0L ||
      (file.exists(lib) && !dir.exists(lib)))
    stop("R library is not writable: ", lib, call. = FALSE)
  normalizePath(lib, winslash = "/", mustWork = create)
}

# Choose a visible compatible/newer version rather than shadowing it with an
# older branch build in a fresh target library. Ties retain library precedence.
.iovrse_resolve <- function(package, libraries) {
  libraries <- unique(libraries)
  versions <- vapply(libraries, function(lib) .iovrse_version(package, lib), character(1))
  found <- which(!is.na(versions))
  if (!length(found)) return(list(version = NA_character_, library = NA_character_))
  best <- found[[1L]]
  for (i in found[-1L]) if (utils::compareVersion(versions[[i]], versions[[best]]) > 0L) best <- i
  list(version = versions[[best]], library = libraries[[best]])
}

# Ordinary imports select the first visible installation, not the highest
# semantic version in every library. Upgrade that installation when necessary.
.iovrse_first_visible <- function(package, libraries) {
  for (library in unique(libraries)) {
    version <- .iovrse_version(package, library)
    if (!is.na(version)) return(list(version = version, library = library))
  }
  list(version = NA_character_, library = NA_character_)
}

# Pure planning does not create a library, load packages or contact repositories.
iovrse_install_plan <- function(packages = names(.iovrse_packages), lib = .libPaths()[1L],
                                visible = .libPaths()) {
  order <- .iovrse_order(packages)
  resolved <- lapply(order, .iovrse_resolve, libraries = unique(c(lib, visible)))
  installed <- vapply(resolved, `[[`, character(1), "version")
  minimum <- vapply(.iovrse_packages[order], `[[`, character(1), "version")
  keep <- mapply(.iovrse_satisfies, installed, minimum)
  data.frame(Package = order, Required = minimum, Installed = installed,
    Action = ifelse(keep, "keep", "install"),
    Library = ifelse(keep, vapply(resolved, `[[`, character(1), "library"), lib),
    Repository = paste0("luciu5/", order),
    Ref = vapply(.iovrse_packages[order], `[[`, character(1), "ref"), row.names = NULL)
}

.iovrse_install_cran <- function(packages, lib, repos, quiet) {
  utils::install.packages(packages, lib = lib, repos = repos,
    dependencies = NA, quiet = quiet)
}

.iovrse_install_remote <- function(package, ref, lib, quiet) {
  # Dependencies are resolved above. Never let a sibling's older Remotes SHA
  # replace a newer, already verified cohort package.
  remotes::install_github(paste0("luciu5/", package), ref = ref, lib = lib,
    dependencies = FALSE, upgrade = "never", build_vignettes = FALSE,
    build_manual = FALSE, quiet = quiet)
}

.iovrse_install_source <- function(path, lib, quiet) {
  # install.packages() returns NULL even after R CMD INSTALL fails. Match its
  # translated nonzero-status notice, preserving every other warning.
  marker <- "IOVRSE_INSTALL_PACKAGE_MARKER"
  notice <- gettextf("installation of package %s had non-zero exit status",
                     marker, domain = "R-utils")
  position <- regexpr(marker, notice, fixed = TRUE)[[1L]]
  prefix <- substr(notice, 1L, position - 1L)
  suffix <- substring(notice, position + nchar(marker))
  withCallingHandlers(
    utils::install.packages(path, lib = lib, repos = NULL, type = "source",
      dependencies = FALSE, quiet = quiet),
    warning = function(condition) {
      message <- conditionMessage(condition)
      if (startsWith(message, prefix) && endsWith(message, suffix) &&
          nchar(message) > nchar(prefix) + nchar(suffix))
        stop("Local source installation failed for ", path, ": ", message,
             call. = FALSE)
    })
}

.iovrse_namespace <- function(package) {
  if (!isNamespaceLoaded(package)) return(NULL)
  list(version = as.character(getNamespaceVersion(package)),
       path = getNamespaceInfo(asNamespace(package), "path"))
}

.iovrse_check_namespace <- function(package, version, library, updating = FALSE) {
  loaded <- .iovrse_namespace(package)
  if (is.null(loaded)) return(invisible(NULL))
  matches <- !updating && identical(loaded$version, version) &&
    identical(.iovrse_path(loaded$path), .iovrse_path(file.path(library, package)))
  if (!matches) stop("Restart R before installing or selecting a different loaded package: ",
    package, ". The loaded namespace differs from the selected installation.", call. = FALSE)
  invisible(NULL)
}

.iovrse_load <- function(package, lib) {
  if (!requireNamespace(package, quietly = TRUE, lib.loc = lib))
    stop("Installed package cannot be loaded: ", package, call. = FALSE)
  .iovrse_check_namespace(package, .iovrse_version(package, lib), lib)
}

# Install a compatible cohort, or a requested subset plus its dependencies.
# source_root is optional developer/CI input; ordinary users need no checkout.
install_iovrse <- function(packages = names(.iovrse_packages), lib = NULL,
                           source_root = NULL,
                           repos = c(STAN = "https://stan-dev.r-universe.dev",
                                     CRAN = "https://cloud.r-project.org"),
                           quiet = FALSE) {
  if (getRversion() < "4.1.0") stop("This cohort requires R >= 4.1.0.", call. = FALSE)
  order <- .iovrse_order(packages)
  if (!is.null(source_root)) {
    source_root <- normalizePath(source_root, mustWork = TRUE)
    for (package in order) {
      path <- file.path(source_root, package, "DESCRIPTION")
      if (!file.exists(path)) stop("Missing local package DESCRIPTION: ", path, call. = FALSE)
      description <- read.dcf(path, fields = c("Package", "Version"))
      if (!identical(unname(description[1L, "Package"]), package) ||
          !.iovrse_satisfies(description[1L, "Version"], .iovrse_packages[[package]]$version))
        stop("Local source does not meet the cohort's package/version contract: ", package, call. = FALSE)
    }
  }
  # All runtime-identity checks happen before library creation or downloads.
  lib <- .iovrse_library(lib, create = FALSE)
  visible <- unique(c(lib, .libPaths()))
  plan <- iovrse_install_plan(order, lib, visible = visible)
  if (!is.null(source_root)) {
    for (package in order) {
      source_version <- read.dcf(file.path(source_root, package, "DESCRIPTION"), fields = "Version")[[1L]]
      installed <- plan$Installed[plan$Package == package]
      if (!is.na(installed) && utils::compareVersion(installed, source_version) > 0L)
        stop("Local source would downgrade ", package,
          "; select matching/newer source or remove the newer library from this development session.", call. = FALSE)
    }
    plan$Action <- "install local source"
    plan$Library <- lib
  }
  needed <- plan$Package[plan$Action != "keep"]
  # R CMD INSTALL prepends its explicit target library when resolving imports.
  # An obsolete cohort copy already there can defeat reuse of a newer visible
  # dependency during installation. Do not overwrite that newer installation.
  if (is.null(source_root) && length(needed)) {
    dependencies <- unique(unlist(lapply(needed, function(package)
      setdiff(.iovrse_order(package), package)), use.names = FALSE))
    for (package in dependencies) {
      row <- plan[plan$Package == package, , drop = FALSE]
      target_version <- .iovrse_version(package, lib)
      if (row$Action == "keep" && !is.na(target_version) &&
          !.iovrse_satisfies(target_version, row$Required))
        stop("Target library shadows the selected compatible ", package,
          " with an older copy. Use a fresh library with lib=PATH, or select ",
          "the compatible dependency library: ", row$Library, call. = FALSE)
    }
  }
  runtime_libraries <- unique(c(lib, plan$Library[plan$Action == "keep"], .libPaths()))
  requirements <- character()
  for (package in order) for (dependency in names(.iovrse_packages[[package]]$cran)) {
    minimum <- .iovrse_packages[[package]]$cran[[dependency]]
    if (!dependency %in% names(requirements) ||
        utils::compareVersion(minimum, requirements[[dependency]]) > 0L)
      requirements[[dependency]] <- minimum
  }
  if (length(needed) && is.null(source_root)) requirements[["remotes"]] <- "0"
  external <- lapply(names(requirements), .iovrse_first_visible, libraries = runtime_libraries)
  names(external) <- names(requirements)
  missing <- names(requirements)[!vapply(names(requirements), function(package)
    .iovrse_satisfies(external[[package]]$version, requirements[[package]]), logical(1))]
  for (i in seq_len(nrow(plan))) .iovrse_check_namespace(plan$Package[[i]],
    plan$Installed[[i]], plan$Library[[i]], updating = plan$Package[[i]] %in% needed)
  for (package in names(requirements)) .iovrse_check_namespace(package,
    external[[package]]$version, external[[package]]$library, updating = package %in% missing)
  print(plan, row.names = FALSE)
  lib <- .iovrse_library(lib, create = TRUE)
  # Prefer the target for new/updated dependencies. Cohort packages kept from
  # other libraries are loaded explicitly below, in dependency order.
  .libPaths(runtime_libraries)
  if (length(missing)) .iovrse_install_cran(missing, lib, repos, quiet)
  for (package in names(requirements)) {
    resolved <- .iovrse_first_visible(package, .libPaths())
    if (!.iovrse_satisfies(resolved$version, requirements[[package]]))
      stop("Dependency is missing or too old after installation: ", package,
        ". Check repository access and supported R versions.", call. = FALSE)
    # Foreign packages may be loaded during another dependency's installation.
    .iovrse_check_namespace(package, resolved$version, resolved$library)
  }
  for (package in needed) {
    if (is.null(source_root))
      .iovrse_install_remote(package, .iovrse_packages[[package]]$ref, lib, quiet) else
      .iovrse_install_source(file.path(source_root, package), lib, quiet)
    if (!.iovrse_satisfies(.iovrse_version(package, lib), .iovrse_packages[[package]]$version))
      stop("Installed ", package, " does not meet the cohort minimum ",
        .iovrse_packages[[package]]$version,
        ". The compatible source release may not yet be published; do not substitute an older core.", call. = FALSE)
  }
  for (i in seq_len(nrow(plan))) .iovrse_load(plan$Package[[i]], plan$Library[[i]])
  # Cohort loading can import external packages that were not loaded during
  # preflight. Verify actual runtime identities before reporting success.
  for (package in names(requirements)) {
    resolved <- .iovrse_first_visible(package, .libPaths())
    .iovrse_check_namespace(package, resolved$version, resolved$library)
  }
  result <- iovrse_install_plan(order, lib)
  message("Compatible R packages are available in the reported libraries. CmdStan setup is needed only for features that run Stan.")
  invisible(result)
}
