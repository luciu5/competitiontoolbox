# A permanent: catches an orphaned server help output or obsolete CRAN URL that
# route/equilibrium tests miss; runs without the optional model stack.
.ct_help_function <- function(file, name, envir) {
  expressions <- parse(ct_app_file(file))
  hit <- vapply(expressions, function(x) is.call(x) &&
    identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name(name)), logical(1))
  stopifnot(sum(hit) == 1L)
  eval(expressions[[which(hit)]], envir)
}

test_that("documentation cards mount current server outputs and online fallback", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- new.env(parent = asNamespace("shiny"))
  .ct_help_function("ui.R", "documentation_page", env)
  .ct_help_function("server.R", "reference_panel", env)
  env$get_vignette_link <- function(...) NULL
  for (case in list(c("antitrust", "Workflow", "referenceATR"),
                    c("trade", "Reference", "referenceTrade"))) {
    card <- as.character(env$documentation_page(case[1], case[3]))
    expect_match(card, paste0('id="', case[3], '"'), fixed = TRUE)
    missing <- as.character(env$reference_panel(case[2], case[1], case[3]))
    expect_match(missing, paste0("vignettes/", case[2], ".Rmd"), fixed = TRUE)
    expect_false(grepl("cran.r-project.org/web/packages", missing, fixed = TRUE))
    env$get_vignette_link <- function(...) "ct-doc/current.html"
    installed <- as.character(env$reference_panel(case[2], case[1], case[3]))
    expect_match(installed, 'src="ct-doc/current.html"', fixed = TRUE)
    env$get_vignette_link <- function(...) NULL
  }
  # Check the mounted navbar, not just a helper that could remain unused.
  ui <- paste(readLines(ct_app_file("ui.R")), collapse = "\n")
  expect_match(ui, 'documentation_page("antitrust", "referenceATR")', fixed = TRUE)
  expect_match(ui, 'documentation_page("trade", "referenceTrade")', fixed = TRUE)
})
