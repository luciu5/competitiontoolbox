app_file <- function(...) {
  source_paths <- c(
    file.path("inst", "ct_shiny", ...),
    file.path("..", "..", "inst", "ct_shiny", ...)
  )
  for (source_path in source_paths) {
    if (file.exists(source_path)) {
      return(source_path)
    }
  }
  system.file("ct_shiny", ..., package = "competitiontoolbox")
}

test_that("model registry exposes supported UI choices", {
  library(antitrust)
  library(trade)
  nPossProds <<- 10
  source(app_file("R", "modelRegistry.R"))

  registry <- model_registry()

  expect_equal(nrow(registry), 34)
  expect_equal(model_demand_choices("Horizontal", "Bertrand", TRUE), c("logit", "ces", "aids"))
  expect_equal(
    model_demand_choices("Horizontal", "Cournot", FALSE),
    c("logit (unknown elasticity)", "linear (unknown elasticity)", "loglinear (unknown elasticity)")
  )
  expect_equal(model_demand_choices("Tariffs", "Monopolistic Competition", TRUE), c("logit", "ces"))
  expect_equal(model_demand_choices("Quotas", "Bertrand", FALSE), "logit (unknown elasticity)")

  expect_true(all(vapply(registry$simulation_fn, exists, logical(1))))
})

test_that("canonical preprocessors preserve required columns", {
  nPossProds <<- 10
  source(app_file("R", "modelRegistry.R"))
  source(app_file("Inputs", "mergersInputs.R"))
  source(app_file("Inputs", "tradeInputs.R"))

  horizontal <- normalize_model_inputs("Horizontal", mergersInputs(type = "Horizontal"))
  tariffs <- normalize_model_inputs("Tariffs", tradeInputs(10, type = "Tariffs"))
  quotas <- normalize_model_inputs("Quotas", tradeInputs(10, type = "Quotas"))
  vertical <- normalize_model_inputs("Vertical", mergersInputs(nrows = 10, type = "Vertical"))

  expect_true(all(c("Prices", "Output", "Margins", "mcDelta") %in% colnames(horizontal)))
  expect_true(all(c("Prices", "Output", "Margins", "tariffPre", "tariffPost", "mcDelta") %in% colnames(tariffs)))
  expect_true(all(c("Prices", "Output", "Margins", "tariffPre", "tariffPost") %in% colnames(quotas)))
  expect_true(all(c("pricesDown", "marginsDown", "pricesUp", "marginsUp") %in% colnames(vertical)))
})

test_that("every registered model runs and tab computations are callable", {
  library(antitrust)
  library(trade)
  nPossProds <<- 10

  source(app_file("R", "modelRegistry.R"))
  source(app_file("Inputs", "mergersInputs.R"))
  source(app_file("Inputs", "tradeInputs.R"))
  source(app_file("Simulations", "mergersSims.R"))
  source(app_file("Simulations", "tradeSims.R"))
  source(app_file("Summary", "mergersSummary.R"))
  source(app_file("Details", "mergersNoPurch.R"))
  source(app_file("Details", "tradeNoPurch.R"))
  source(app_file("Diagnostics", "mergersDiag.R"))
  source(app_file("Diagnostics", "tradeDiag.R"))

  registry <- model_registry()
  for (i in seq_len(nrow(registry))) {
    spec <- registry[i, ]
    input_data <- switch(
      spec$page,
      Horizontal = mergersInputs(type = "Horizontal"),
      Vertical = mergersInputs(nrows = 10, type = "Vertical"),
      Tariffs = tradeInputs(10, type = "Tariffs"),
      Quotas = tradeInputs(10, type = "Quotas")
    )
    sim_fun <- if (spec$page %in% c("Horizontal", "Vertical")) mergersSims else tradeSims

    sim <- suppressWarnings(sim_fun(spec$supply, spec$demand_label, input_data, -1, spec$page))

    expect_s4_class(sim, class(sim)[1])
    expect_identical(attr(sim, "ct_model_spec")$supply, spec$supply)
    expect_error(capture.output(suppressWarnings(summary(sim, market = TRUE))), NA)
    expect_error(capture.output(suppressWarnings(summary(sim, market = FALSE))), NA)
    expect_error(format_elasticity_table(elast(sim, preMerger = TRUE, market = FALSE), sim), NA)
    expect_error(elast(sim, preMerger = TRUE, market = TRUE), NA)
    expect_error(getParms(sim, digits = 2), NA)

    if (spec$page %in% c("Horizontal", "Vertical")) {
      expect_error(mergersDiag(sim), NA)
      expect_error(mergersDiag(sim, mktElast = TRUE), NA)
      expect_error(mergersNoPurch(sim), NA)
    } else {
      expect_error(tradeDiag(sim), NA)
      expect_error(tradeDiag(sim, mktElast = TRUE), NA)
      expect_error(tradeNoPurch(sim), NA)
    }
  }
})

test_that("generated R code parses and representative snippets execute", {
  library(antitrust)
  library(trade)
  nPossProds <<- 10

  source(app_file("R", "modelRegistry.R"))
  source(app_file("Inputs", "mergersInputs.R"))
  source(app_file("Inputs", "tradeInputs.R"))
  source(app_file("R", "mergersTemplateCode.R"))
  source(app_file("R", "tradeTemplateCode.R"))

  values <<- list(inputData = mergersInputs(type = "Horizontal"))
  valuesVertical <<- list(inputData = mergersInputs(nrows = 10, type = "Vertical"))
  valuesTariffs <<- list(inputData = tradeInputs(10, type = "Tariffs"))
  valuesQuota <<- list(inputData = tradeInputs(10, type = "Quotas"))
  input <<- list(
    calcElast = "market elasticity",
    enterElast = -1,
    calcElastTariffs = "market elasticity",
    enterElastTariffs = -1,
    calcElastQuota = "market elasticity",
    enterElastQuota = -1,
    supplyTariffs = "Bertrand",
    supplyQuota = "Bertrand"
  )
  supply <<- function() "Bertrand"
  demand <<- function() "logit"

  code_by_page <- list(
    Horizontal = mergersTemplateCode("Horizontal"),
    Vertical = mergersTemplateCode("Vertical"),
    Tariffs = tradeTemplateCode("Tariffs"),
    Quotas = tradeTemplateCode("Quotas")
  )

  for (code in code_by_page) {
    text <- paste(code, collapse = "\n")
    expect_silent(parse(text = text))
    expect_error(capture.output(suppressWarnings(eval(parse(text = text)))), NA)
  }
})
