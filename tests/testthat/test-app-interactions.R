test_that("A: copied R code reproduces edited UI economics in a clean R process", {
  # The former evaluation environment inherited application objects. A snippet
  # could therefore pass while depending on Shiny state or stale default data.
  ct_load_core(include_templates = TRUE)
  directory <- tempfile("ct-standalone-"); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  ct_app_env$input <- list(calcElast = "market elasticity", enterElast = -1,
    calcElastTariffs = "market elasticity", enterElastTariffs = -1,
    calcElastQuota = "market elasticity", enterElastQuota = -1)
  ct_app_env$supply <- function() "Bertrand"
  ct_app_env$demand <- function() "logit"
  for (page in c("Horizontal", "Vertical", "Tariffs", "Quotas")) {
    data <- ct_default_inputs(page)
    data$Name[1L] <- "Product 'one'"
    if (page == "Horizontal") data[[grep("Cost Changes", names(data))]][1L] <- -.04
    if (page == "Vertical") {
      data <- ct_app_env$mergersInputs(10L, "Vertical", "Downstream")
      data$Name[1L] <- "Product 'one'"
    }
    if (page == "Tariffs") data[[grep("^New", names(data))]][1:2] <- .15
    if (page == "Quotas") data[[grep("^New", names(data))]][1:2] <- .8
    field <- switch(page, Horizontal = "values", Vertical = "valuesVertical",
                    Tariffs = "valuesTariffs", Quotas = "valuesQuota")
    ct_app_env[[field]] <- list(inputData = data)
    row <- ct_app_env$model_registry()
    row <- row[row$page == page & row$supply == "Bertrand" & row$demand_label == "logit", , drop = FALSE]
    expected <- suppressWarnings(ct_run_row(row, data))
    code <- if (page %in% c("Horizontal", "Vertical")) ct_app_env$mergersTemplateCode(page) else ct_app_env$tradeTemplateCode(page)
    object <- if (page == "Vertical") "simres_vert" else "simres"
    package <- row$upstream_package
    script <- file.path(directory, paste0(page, ".R"))
    result <- file.path(directory, paste0(page, ".rds"))
    writeLines(c(code, paste0("saveRDS(list(prices=", package, "::calcPrices(", object,
      ",preMerger=FALSE),shares=", package, "::calcShares(", object,
      ",preMerger=FALSE)),", encodeString(result, quote = '"'), ")")), script)
    log <- file.path(directory, paste0(page, ".log"))
    status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = log, stderr = log)
    expect_equal(status, 0L, info = paste(page, paste(readLines(log), collapse = "\n")))
    if (status != 0L) next
    actual <- readRDS(result)
    price <- getExportedValue(package, "calcPrices")
    shares <- getExportedValue(package, "calcShares")
    expect_equal(ct_unname_recursive(actual$prices), ct_unname_recursive(price(expected, preMerger = FALSE)), tolerance = 1e-7, info = page)
    expect_equal(ct_unname_recursive(actual$shares), ct_unname_recursive(shares(expected, preMerger = FALSE)), tolerance = 1e-7, info = page)
  }
})

test_that("A: actual Shiny session routes pages and recovers from invalid edited inputs", {
  # Static route tests miss reactive wiring, event buttons, page state and error
  # rendering. This exercises the actual server without claiming browser coverage.
  ct_require_stack()
  app_dir <- dirname(ct_app_file("server.R"))
  shiny::testServer(shiny::shinyAppDir(normalizePath(app_dir)), {
    session$setInputs(menu = "Horizontal", supply = "Bertrand", supplyTariffs = "Bertrand",
      supplyQuota = "Bertrand", supplyVertical = "Bertrand", demand1 = "logit",
      demandTariffs1 = "logit", demandQuota1 = "logit", demandVertical1 = "logit",
      calcElast = "market elasticity AND 1 or more margins", enterElast = -1,
      calcElastTariffs = "market elasticity AND 1 or more margins", enterElastTariffs = -1,
      calcElastQuota = "market elasticity AND 1 or more margins", enterElastQuota = -1,
      addRowsTariffs = 10L, addRowsQuota = 10L, addRowsVertical = 10L,
      mergerTypeVertical = "Upstream", simulate = 0L, simulateVertical = 0L,
      simulateTariffs = 0L, simulateQuota = 0L, inTabset = "respanel",
      inTabsetVertical = "respanelVertical", inTabsetTariffs = "respanelTariffs", inTabsetQuota = "respanelQuota")
    session$setInputs(simulate = 1L)
    expect_null(values$msg$error)
    expect_true(all(is.finite(antitrust::calcPrices(values$sim, preMerger = FALSE))))
    baseline <- antitrust::calcPrices(values$sim, preMerger = FALSE)
    valid <- values$inputData
    invalid <- valid; invalid[[grep("^Prices", names(invalid))]][1L] <- -1
    values$inputData <- invalid
    session$setInputs(simulate = 2L)
    expect_null(values$sim)
    expect_true(length(values$msg$error) > 0L)
    session$setInputs(inTabset = "msgpanel")
    expect_match(output$errors, "price", ignore.case = TRUE)
    values$inputData <- valid
    session$setInputs(simulate = 3L)
    expect_null(values$msg$error)
    expect_equal(antitrust::calcPrices(values$sim, preMerger = FALSE), baseline)
    session$setInputs(menu = "Tariffs", simulateTariffs = 1L)
    expect_null(valuesTariffs$msg$error)
    expect_true(all(is.finite(trade::calcPrices(valuesTariffs$sim, preMerger = FALSE))))
    session$setInputs(menu = "Quotas", simulateQuota = 1L)
    expect_null(valuesQuota$msg$error)
    expect_true(all(is.finite(trade::calcPrices(valuesQuota$sim, preMerger = FALSE))))
    session$setInputs(menu = "Vertical", simulateVertical = 1L)
    expect_null(valuesVertical$msg$error)
    expect_true(all(is.finite(unlist(vertical::calcPrices(valuesVertical$sim, preMerger = FALSE)))))
    expect_equal(antitrust::calcPrices(values$sim, preMerger = FALSE), baseline)
    # No-vignette development installs should present an explicit message;
    # complete builds should link the current Workflow rather than old Reference.
    expect_match(output$referenceATR$html, "Workflow|iframe")
  })
})
