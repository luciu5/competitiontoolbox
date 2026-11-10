# Material failures: dependency recursion can downgrade the cohort; disk version
# checks can report success while R is still using a stale loaded namespace.
local({
  candidates <- c('tools/install_iovrse.R', '../tools/install_iovrse.R',
                  '../../tools/install_iovrse.R')
  checkout <- candidates[file.exists(candidates)]
  if (length(checkout)) {
    canonical <- normalizePath(checkout[[1L]])
    copy <- file.path(dirname(dirname(canonical)), 'inst/install_iovrse.R')
    stopifnot(file.exists(copy), identical(readBin(canonical, 'raw', file.info(canonical)$size),
                                         readBin(copy, 'raw', file.info(copy)$size)))
  }
  override <- Sys.getenv('IOVRSE_INSTALLER', '')
  script <- if (nzchar(override)) override else if (length(checkout)) checkout[[1L]] else
    system.file('install_iovrse.R', package = 'competitiontoolbox')
  stopifnot(nzchar(script), file.exists(script))
  original <- new.env(parent = globalenv()); source(script, local = original)
  fixture <- tempfile('iovrse-installer-'); dir.create(fixture)
  # Match the installer's canonical paths, including Windows separators and
  # macOS /var -> /private/var aliases, before indexing mock libraries.
  fixture <- normalizePath(fixture, winslash = '/', mustWork = TRUE)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  target <- file.path(fixture, 'target'); dir.create(target)
  visible <- file.path(fixture, 'visible'); dir.create(visible)
  libraries <- .libPaths(); on.exit(.libPaths(libraries), add = TRUE)
  .libPaths(c(visible, libraries))
  fails <- function(code, message) {
    error <- tryCatch(force(code), error = identity)
    stopifnot(inherits(error, 'error'), grepl(message, conditionMessage(error), fixed = TRUE))
  }
  fresh <- function() {
    e <- new.env(parent = globalenv()); source(script, local = e)
    e$installed <- list(); e$namespaces <- list(); e$calls <- list(); e$loads <- list()
    e$set_version <- function(package, version, lib = target) {
      values <- e$installed[[lib]]; values[[package]] <- version; e$installed[[lib]] <- values
    }
    e$.iovrse_version <- function(package, lib) {
      for (library in lib) {
        value <- e$installed[[library]][[package]]
        if (!is.null(value)) return(value)
      }
      NA_character_
    }
    e$.iovrse_namespace <- function(package) e$namespaces[[package]]
    e$.iovrse_install_cran <- function(packages, lib, repos, quiet) {
      e$calls[[length(e$calls)+1L]] <- list(kind='dependencies', packages=packages)
      for (package in packages) e$set_version(package, '99.0.0', lib)
    }
    e$.iovrse_install_remote <- function(package, ref, lib, quiet) {
      e$calls[[length(e$calls)+1L]] <- list(kind='remote', package=package, ref=ref)
      e$set_version(package, e$.iovrse_packages[[package]]$version, lib)
    }
    e$.iovrse_install_source <- function(path, lib, quiet) {
      package <- basename(path)
      e$calls[[length(e$calls)+1L]] <- list(kind='source', package=package)
      e$set_version(package, read.dcf(file.path(path,'DESCRIPTION'), fields='Version')[[1L]],lib)
    }
    e$.iovrse_load <- function(package, lib) {
      stopifnot(e$.iovrse_satisfies(e$.iovrse_version(package,lib), e$.iovrse_packages[[package]]$version))
      e$loads[[package]] <- lib
    }
    e
  }
  e <- fresh()
  stopifnot(identical(e$.iovrse_order('financial'),c('antitrust','financial')),
    identical(e$.iovrse_order('antitrustBayes'),c('antitrust','coordination','antitrustBayes')))
  fails(e$.iovrse_order('unimplemented'),'Choose package names')
  e$install_iovrse('financial',lib=target,quiet=TRUE)
  remote <- Filter(function(x)x$kind=='remote',e$calls)
  stopifnot(identical(vapply(remote,`[[`,character(1),'package'),c('antitrust','financial')),
    is.null(e$installed[[target]][['cmdstanr']]),
    all(c('BB','numDeriv','nleqslv','SQUAREM','cubature','dplyr') %in% names(e$installed[[target]])))

  # A fresh target must reuse a newer visible core, not install a lower shadow.
  e <- fresh(); e$set_version('antitrust','99.0.0',visible)
  e$install_iovrse(c('antitrustBayes','competitiontoolbox'),lib=target,quiet=TRUE)
  remote <- Filter(function(x)x$kind=='remote',e$calls)
  stopifnot(!'antitrust' %in% vapply(remote,`[[`,character(1),'package'),
    is.null(e$installed[[target]][['antitrust']]), identical(e$loads$antitrust,visible),
    !is.null(e$installed[[target]][['cmdstanr']]))

  # Compatible disks do not make an older/different loaded namespace current.
  for (mismatch in c('version','path')) {
    e <- fresh(); e$set_version('antitrust','99.0.0',visible)
    e$namespaces$antitrust <- list(version=if(mismatch=='version') '0.99.33' else '99.0.0',
      path=file.path(if(mismatch=='path') target else visible,'antitrust'))
    absent <- file.path(fixture,paste0('not-created-',mismatch))
    before <- .libPaths()
    fails(e$install_iovrse('antitrust',lib=absent,quiet=TRUE),'Restart R')
    stopifnot(!dir.exists(absent),!length(e$calls),identical(.libPaths(),before))
  }
  # A matching loaded namespace may be kept; no unconditional restart gate.
  e <- fresh(); e$set_version('antitrust','99.0.0',visible)
  e$namespaces$antitrust <- list(version='99.0.0',path=file.path(visible,'antitrust'))
  e$install_iovrse('antitrust',lib=target,quiet=TRUE)
  stopifnot(!length(Filter(function(x)x$kind=='remote',e$calls)))

  # Updating or reselecting a foreign dependency must not leave a stale DLL/code.
  for (mismatch in c('upgrade','path')) {
    e <- fresh(); e$set_version('cmdstanr',if(mismatch=='upgrade') '0.8.0' else '0.9.0',visible)
    e$namespaces$cmdstanr <- list(version=if(mismatch=='upgrade') '0.8.0' else '0.9.0',
      path=file.path(if(mismatch=='path') target else visible,'cmdstanr'))
    fails(e$install_iovrse('antitrustBayes',lib=target,quiet=TRUE),'Restart R')
    stopifnot(!length(e$calls))
  }
  # A deleted ordinary dependency can still be repaired without reinstalling fits/packages.
  e <- fresh(); e$set_version('antitrust','99.0.0'); e$set_version('financial','0.1.0')
  for(p in c('BB','numDeriv','nleqslv','SQUAREM','dplyr')) e$set_version(p,'99.0.0')
  e$install_iovrse('financial',lib=target,quiet=TRUE)
  stopifnot(length(e$calls)==1L,identical(e$calls[[1L]]$packages,'cubature'))

  # A stale branch is not a successful install merely because the adapter returned.
  e <- fresh(); e$.iovrse_install_remote <- function(package,ref,lib,quiet)e$set_version(package,'0.99.43',lib)
  fails(e$install_iovrse('antitrust',lib=target,quiet=TRUE),'does not meet the cohort minimum')

  # Local source remains an explicit reinstall; actual version downgrades are rejected.
  local_root <- file.path(fixture,'sources'); dir.create(local_root)
  dir.create(file.path(local_root,'antitrust'))
  writeLines(c('Package: antitrust','Version: 0.99.43.9000'),file.path(local_root,'antitrust','DESCRIPTION'))
  e <- fresh(); e$set_version('antitrust','99.0.0',visible)
  fails(e$install_iovrse('antitrust',lib=target,source_root=local_root,quiet=TRUE),'would downgrade')
  stopifnot(!length(e$calls))
  e <- fresh(); e$install_iovrse('antitrust',lib=target,source_root=local_root,quiet=TRUE)
  stopifnot(length(Filter(function(x)x$kind=='source',e$calls))==1L)

  # External dependencies follow actual import precedence. A compatible loaded
  # first-visible version must not be rejected merely because a later copy is newer.
  e <- fresh(); e$set_version('antitrust','99.0.0',target)
  e$set_version('BB','1.0.0',target); e$set_version('BB','99.0.0',visible)
  e$namespaces$BB <- list(version='1.0.0',path=file.path(target,'BB'))
  e$install_iovrse('antitrust',lib=target,quiet=TRUE)
  stopifnot(identical(e$.iovrse_first_visible('BB',c(target,visible))$version,'1.0.0'),
    !any(vapply(e$calls,function(x)'BB' %in% x$packages,logical(1))))

  # Local reinstalls keep the target first even when ordinary dependencies are
  # kept elsewhere beside an obsolete cohort copy.
  dir.create(file.path(local_root,'coordination'))
  writeLines(c('Package: coordination','Version: 0.1.0'),file.path(local_root,'coordination','DESCRIPTION'))
  e <- fresh(); e$set_version('antitrust','0.99.34',visible)
  for(p in c('BB','numDeriv','nleqslv','SQUAREM'))e$set_version(p,'99.0.0',visible)
  install_source <- e$.iovrse_install_source
  e$.iovrse_install_source <- function(path,lib,quiet) {
    stopifnot(identical(.libPaths()[1L],target))
    if(basename(path)=='coordination')stopifnot(e$.iovrse_version('antitrust',.libPaths())=='0.99.43.9000')
    install_source(path,lib,quiet)
  }
  e$install_iovrse('coordination',lib=target,source_root=local_root,quiet=TRUE)

  # R CMD INSTALL would put an obsolete target core before the selected newer
  # visible core. Explain this precise conflict before downloads or writes.
  e <- fresh();e$set_version('antitrust','0.99.34',target)
  e$set_version('antitrust','0.99.43.9000',visible)
  fails(e$install_iovrse('financial',lib=target,quiet=TRUE),'Target library shadows')
  stopifnot(!length(e$calls))

  # A foreign namespace first imported by a cohort package still needs its
  # identity checked before the installer can claim success.
  e <- fresh();e$set_version('antitrust','99.0.0',target)
  load_cohort <- e$.iovrse_load
  e$.iovrse_load <- function(package,lib) {
    load_cohort(package,lib)
    e$namespaces$BB <- list(version='0.0.1',path=file.path(visible,'BB'))
  }
  fails(e$install_iovrse('antitrust',lib=target,quiet=TRUE),'Restart R')

  # requireNamespace() alone accepts an already loaded namespace even when the
  # requested library is absent. Final verification must reject that false pass.
  original$.iovrse_load('stats', dirname(find.package('stats')))
  fails(original$.iovrse_load('stats',file.path(fixture,'absent-library')),'Restart R')

  # Execute the real adapter, replacing only its external call target by a recorder.
  # This catches omitted ref/lib/quiet arguments and accidentally recursive deps.
  adapter <- original$.iovrse_install_remote
  replace_remote <- function(x) {
    if (!is.call(x)) return(x)
    if (is.call(x[[1L]]) && identical(x[[1L]],quote(remotes::install_github)))
      x[[1L]] <- as.name('record_remote')
    for(i in seq_along(x)[-1L]) if(is.call(x[[i]]))x[[i]] <- replace_remote(x[[i]])
    x
  }
  body(adapter) <- replace_remote(body(adapter))
  recorded <- NULL
  original$record_remote <- function(...) {recorded <<- list(...);'recorded'}
  stopifnot(identical(adapter('trade','master',target,TRUE),'recorded'),
    identical(recorded[[1L]],'luciu5/trade'), identical(recorded$ref,'master'),
    identical(recorded$lib,target),identical(recorded$dependencies,FALSE),
    identical(recorded$upgrade,'never'),identical(recorded$build_vignettes,FALSE),
    identical(recorded$build_manual,FALSE),identical(recorded$quiet,TRUE))

  # Execute the local adapter too: install.packages returns NULL after a failed
  # install, potentially leaving an old equal-version package that passes a
  # version-only check. Convert only its known nonzero-status warning to error.
  source_adapter <- original$.iovrse_install_source
  replace_source <- function(x) {
    if (!is.call(x)) return(x)
    if (is.call(x[[1L]]) && identical(x[[1L]],quote(utils::install.packages)))
      x[[1L]] <- as.name('record_source')
    for(i in seq_along(x)[-1L])if(is.call(x[[i]]))x[[i]] <- replace_source(x[[i]])
    x
  }
  body(source_adapter) <- replace_source(body(source_adapter))
  original$record_source <- function(...) {
    warning(gettextf('installation of package %s had non-zero exit status',
      sQuote('C:/temporary source/antitrust'),domain='R-utils'),call.=FALSE)
    NULL
  }
  e <- fresh();e$set_version('antitrust','0.99.43.9000',target)
  e$.iovrse_install_source <- source_adapter
  fails(e$install_iovrse('antitrust',lib=target,source_root=local_root,quiet=TRUE),
    'Local source installation failed')
  stopifnot(identical(e$.iovrse_version('antitrust',target),'0.99.43.9000'))
  original$record_source <- function(...) {
    warning('nonfatal local source warning fixture',call.=FALSE)
    NULL
  }
  retained <- character()
  withCallingHandlers(source_adapter(file.path(local_root,'antitrust'),target,TRUE),
    warning=function(w){retained <<- c(retained,conditionMessage(w));invokeRestart('muffleWarning')})
  stopifnot(identical(retained,'nonfatal local source warning fixture'))
  cat('Installer subset, visible-version, namespace, local-source and executed-adapter regressions passed.\n')
})
