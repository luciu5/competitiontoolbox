## High-value integration cases that exercise inputs beyond the default Shiny
## fixtures.  Each migrated route is compared with a separately shaped call
## to the upstream package's public legacy constructor; the route itself is
## therefore not used as its own oracle.

ct_expect_nondefault_parity <- function(result, oracle, package, tolerance = 1e-8) {
  for (method in c("calcPrices", "calcShares", "calcMargins", "calcQuantities")) {
    actual <- getExportedValue(package, method)(result, preMerger = FALSE)
    expected <- getExportedValue(package, method)(oracle, preMerger = FALSE)
    testthat::expect_equal(
      ct_unname_recursive(actual),
      ct_unname_recursive(expected),
      tolerance = tolerance,
      info = paste(package, method)
    )
  }
  invisible(TRUE)
}

test_that("horizontal routes preserve custom ownership and marginal-cost transitions", {
  ct_load_core()
  registry <- ct_app_env$model_registry()
  row <- registry[
    registry$page == "Horizontal" &
      registry$supply == "Bertrand" &
      registry$demand_label == "logit" &
      registry$calibration == "known",
    , drop = FALSE
  ]
  expect_equal(nrow(row), 1L)

  data <- ct_complete_data(ct_default_inputs("Horizontal"))
  pre_owner <- grep("^Pre-merger", names(data), value = TRUE)
  post_owner <- grep("^Post-merger", names(data), value = TRUE)[1L]
  cost_change <- grep("Cost Changes", names(data), value = TRUE)
  data[[pre_owner]] <- c("A", "A", "B", "C")
  data[[post_owner]] <- c("A", "B", "B", "B")
  data[[cost_change]] <- c(0.10, -0.05, 0.20, 0.00)

  result <- suppressWarnings(ct_run_row(row, data))
  oracle <- ct_legacy_model(row, data)
  inputs <- attr(result, "ct_model_inputs", exact = TRUE)

  expect_equal(as.character(inputs[[pre_owner]]), data[[pre_owner]])
  expect_equal(as.character(inputs[[post_owner]]), data[[post_owner]])
  expect_equal(as.numeric(inputs$mcDelta), c(0.10, -0.05, 0.20, 0.00))
  ct_expect_nondefault_parity(result, oracle, "antitrust")
})

test_that("asymmetric tariff and quota policy changes use public trade outcomes", {
  ct_load_core()
  registry <- ct_app_env$model_registry()

  cases <- list(
    Tariffs = list(
      pre = c(0.05, 0.02, 0.10, 0.10),
      post = c(0.35, 0.00, 0.20, 0.20),
      tolerance = 1e-8
    ),
    Quotas = list(
      pre = c(Inf, Inf, Inf, Inf),
      post = c(0.50, Inf, 0.20, 0.40),
      tolerance = 1e-7
    )
  )

  for (page in names(cases)) {
    row <- registry[
      registry$page == page &
        registry$supply == "Bertrand" &
        registry$demand_label == "logit" &
        registry$calibration == "known",
      , drop = FALSE
    ]
    expect_equal(nrow(row), 1L, info = page)

    data <- ct_complete_data(ct_default_inputs(page))
    margins <- grep("^Margins", names(data), value = TRUE)
    pre_policy <- grep("^Current", names(data), value = TRUE)
    post_policy <- grep("^New", names(data), value = TRUE)
    ## The default fixture leaves two margins blank.  Supplying all four here
    ## makes the parity assertion cover the complete post-policy output.
    data[[margins]] <- c(0.25, 0.25, 0.20, 0.20)
    data[[pre_policy]] <- cases[[page]]$pre
    data[[post_policy]] <- cases[[page]]$post

    result <- suppressWarnings(ct_run_row(row, data))
    oracle <- ct_legacy_model(row, data)
    inputs <- attr(result, "ct_model_inputs", exact = TRUE)

    expect_equal(as.numeric(inputs$policyPre), cases[[page]]$pre,
                 info = paste(page, "pre-policy"))
    expect_equal(as.numeric(inputs$policyPost), cases[[page]]$post,
                 info = paste(page, "post-policy"))
    pre_prices <- trade::calcPrices(result, preMerger = TRUE)
    post_prices <- trade::calcPrices(result, preMerger = FALSE)
    expect_true(any(abs(unlist(post_prices) - unlist(pre_prices)) > 1e-6),
                info = paste(page, "policy changes equilibrium prices"))
    ct_expect_nondefault_parity(
      result, oracle, "trade", tolerance = cases[[page]]$tolerance
    )
  }
})

test_that("partially populated UI grids drop empty rows without changing outputs", {
  ct_load_core()
  registry <- ct_app_env$model_registry()

  blank_row <- function(data) {
    blank <- data[1L, , drop = FALSE]
    blank[] <- NA
    blank
  }

  for (page in c("Horizontal", "Tariffs")) {
    row <- registry[
      registry$page == page &
        registry$supply == "Bertrand" &
        registry$demand_label == "logit" &
        registry$calibration == "known",
      , drop = FALSE
    ]
    expect_equal(nrow(row), 1L, info = page)

    base <- ct_complete_data(ct_default_inputs(page))
    if (page == "Tariffs") {
      margins <- grep("^Margins", names(base), value = TRUE)
      base[[margins]] <- c(0.25, 0.20, 0.15, 0.10)
    }
    ## Keep three populated products, interleave explicit empty UI rows, and
    ## include a named row whose missing output makes it economically unusable.
    named_missing_output <- base[4L, , drop = FALSE]
    output_name <- grep("Quantities|Revenues", names(base), value = TRUE)[1L]
    named_missing_output[[output_name]] <- NA_real_
    sparse <- rbind(
      base[1L, , drop = FALSE], blank_row(base),
      base[2L, , drop = FALSE], named_missing_output, blank_row(base),
      base[3L, , drop = FALSE], blank_row(base)
    )

    result <- suppressWarnings(ct_run_row(row, sparse))
    oracle <- ct_legacy_model(row, base[1:3, , drop = FALSE])
    inputs <- attr(result, "ct_model_inputs", exact = TRUE)

    expect_equal(nrow(inputs), 3L, info = paste(page, "retained rows"))
    expect_equal(inputs$Name, base$Name[1:3],
                 info = paste(page, "retained product order"))
    expect_false(base$Name[4L] %in% inputs$Name,
                 info = paste(page, "named missing-output row dropped"))
    expect_true(all(!is.na(inputs$Output)), info = paste(page, "no empty output"))
    package <- if (page == "Horizontal") "antitrust" else "trade"
    ct_expect_nondefault_parity(result, oracle, package)
  }
})

test_that("vertical routes preserve custom upstream and downstream ownership transitions", {
  ct_load_core()
  registry <- ct_app_env$model_registry()
  row <- registry[
    registry$page == "Vertical" &
      registry$supply == "Bertrand" &
      registry$demand_label == "logit",
    , drop = FALSE
  ]
  expect_equal(nrow(row), 1L)

  data <- ct_complete_data(ct_default_inputs("Vertical"))
  data$ownerPostUp <- c("U1", "U1", "U2", "U2")
  data$ownerPostDown <- c("D1", "D2", "D1", "D2")

  result <- suppressWarnings(ct_run_row(row, data))
  oracle <- ct_legacy_model(row, data)
  inputs <- attr(result, "ct_model_inputs", exact = TRUE)
  expect_equal(as.character(inputs$ownerPostUp), data$ownerPostUp)
  expect_equal(as.character(inputs$ownerPostDown), data$ownerPostDown)

  for (pre_merger in c(TRUE, FALSE)) {
    for (method in c("calcPrices", "calcShares", "calcMargins")) {
      fun <- getExportedValue("vertical", method)
      actual <- fun(result, preMerger = pre_merger)
      expected <- fun(oracle, preMerger = pre_merger)
      expect_equal(
        ct_unname_recursive(actual),
        ct_unname_recursive(expected),
        tolerance = 1e-7,
        info = paste("vertical", method, "preMerger", pre_merger)
      )
    }
  }
})
