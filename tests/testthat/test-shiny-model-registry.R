test_that("the UI registry has 34 rows and complete public mappings", {
  ct_load_core()
  registry <- ct_app_env$model_registry()

  expect_equal(nrow(registry), 34L)
  expect_equal(
    as.integer(table(registry$page)[c("Horizontal", "Quotas", "Tariffs", "Vertical")]),
    unname(c(Horizontal = 14L, Quotas = 2L, Tariffs = 16L, Vertical = 2L))
  )
  expect_true(all(c(
    "page", "supply", "demand_label", "demand_id", "calibration",
    "upstream_package", "conduct", "variant", "policy"
  ) %in% names(registry)))

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]
    spec <- ct_expect_no_error(
      ct_app_env$model_spec(row$page, row$supply, row$demand_label),
      info = paste("registry row", i)
    )

    expect_equal(spec$upstream_package, row$upstream_package)
    expect_equal(spec$conduct, row$conduct)
    expect_equal(spec$variant, row$variant)
    if (is.na(row$policy)) {
      expect_true(is.na(spec$policy) || is.null(spec$policy))
    } else {
      expect_equal(spec$policy, row$policy)
    }

    ## Check against each upstream package's exported registry as well as its
    ## constructor.  This catches a locally plausible but unsupported
    ## demand/conduct/variant combination.
    upstream <- ct_upstream_spec(row)
    ct_same_spec(spec$upstream_spec, upstream)
    public <- switch(
      row$upstream_package,
      antitrust = antitrust::supportedModels(),
      trade = trade::supportedModels(),
      vertical = vertical::supportedModels()
    )
    matches <- public[
      public$demand == upstream$demand &
        public$conduct == upstream$conduct &
        public$variant == upstream$variant,
      , drop = FALSE
    ]
    if (row$upstream_package == "trade") {
      matches <- matches[matches$policy == upstream$policy, , drop = FALSE]
    }
    expect_equal(nrow(matches), 1L, info = paste("public registry row", i))
    if ("id" %in% names(matches)) {
      expect_equal(as.character(upstream$id), as.character(matches$id))
    } else {
      ## rbind.data.frame() may replace the source row names in the upstream
      ## registry; the normalized fields above are the public identity.
      expect_true(nzchar(upstream$id))
    }
  }
})

test_that("input normalization exposes the canonical route fields", {
  ct_load_core()

  horizontal <- ct_default_inputs("Horizontal")
  tariffs <- ct_default_inputs("Tariffs")
  quotas <- ct_default_inputs("Quotas")
  vertical <- ct_default_inputs("Vertical")

  horizontal <- ct_app_env$normalize_model_inputs("Horizontal", horizontal)
  tariffs <- ct_app_env$normalize_model_inputs("Tariffs", tariffs)
  quotas <- ct_app_env$normalize_model_inputs("Quotas", quotas)
  vertical <- ct_app_env$normalize_model_inputs("Vertical", vertical)

  expect_true(all(c("Prices", "Output", "Margins", "mcDelta") %in%
                    colnames(horizontal)))
  expect_true(all(c("Prices", "Output", "Margins", "policyPre", "policyPost") %in%
                    colnames(tariffs)))
  expect_true(all(c("Prices", "Output", "Margins", "policyPre", "policyPost") %in%
                    colnames(quotas)))
  expect_true(all(c("pricesDown", "marginsDown", "pricesUp", "marginsUp") %in%
                    colnames(vertical)))
  expect_equal(tariffs$policyPre[seq_len(4L)], c(.05, .05, 0, 0))
  expect_equal(quotas$policyPre[seq_len(4L)], rep(Inf, 4L))
  expect_equal(quotas$policyPost[seq_len(4L)], c(.75, .75, Inf, Inf))
})

test_that("every UI row runs with defaults and tab computations remain callable", {
  ct_load_core()
  registry <- ct_app_env$model_registry()

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]
    data <- ct_default_inputs(row$page)
    result <- ct_expect_no_error(
      ct_run_row(row, data),
      info = paste("model route row", i, row$page, row$supply,
                   row$demand_label)
    )

    expect_true(isS4(result))
    fit <- attr(result, "ct_fit", exact = TRUE)
    spec <- attr(result, "ct_model_spec", exact = TRUE)
    inputs <- attr(result, "ct_model_inputs", exact = TRUE)

    expect_s4_class(fit, ct_fit_class(row))
    expect_false(methods::is(result, ct_fit_class(row)))
    expect_equal(spec$page, row$page)
    expect_equal(spec$upstream_package, row$upstream_package)
    ct_same_spec(spec$upstream_spec, ct_upstream_spec(row))
    expect_true(is.data.frame(inputs))
    expect_gt(nrow(inputs), 0L)
    expect_identical(attr(result, "ct_fit", exact = TRUE), fit)
    expected_api <- if (row$upstream_package == "antitrust") {
      "antitrust::simulate"
    } else if (row$upstream_package == "vertical") {
      "vertical::simulate"
    } else {
      "trade::simulate"
    }
    expect_identical(
      attr(result, "ct_simulation_api", exact = TRUE), expected_api
    )

    ## The tabs use the raw one-step S4 result.  Call the package owning the
    ## model explicitly because antitrust, trade, and vertical re-export
    ## several methods with the same names.
    summary_fun <- switch(
      row$upstream_package,
      antitrust = antitrust::summary,
      trade = trade::summary,
      vertical = vertical::summary
    )
    ct_expect_no_error(capture.output(summary_fun(result, market = TRUE)),
                    info = paste("summary row", i))
    ct_expect_no_error(capture.output(summary_fun(result, market = FALSE)),
                    info = paste("market summary row", i))

    if (row$page == "Horizontal") {
      ct_expect_no_error(ct_app_env$mergersSummary(result),
                      info = paste("merger summary row", i))
      ct_expect_no_error(ct_app_env$mergersDiag(result),
                      info = paste("merger diagnostics row", i))
      ct_expect_no_error(ct_app_env$mergersDiag(result, mktElast = TRUE),
                      info = paste("merger elasticity diagnostics row", i))
      ct_expect_no_error(ct_app_env$mergersNoPurch(result),
                      info = paste("merger details row", i))
    } else if (row$page == "Vertical") {
      ct_expect_no_error(capture.output(vertical::summary(result, market = TRUE)),
                         info = paste("vertical summary row", i))
      ct_expect_no_error(ct_app_env$mergersDiag(result),
                         info = paste("vertical diagnostics row", i))
      ct_expect_no_error(ct_app_env$mergersNoPurch(result),
                         info = paste("vertical details row", i))
    } else {
      ct_expect_no_error(ct_app_env$tradeMarketReport(result),
                      info = paste("trade market report row", i))
      ct_expect_no_error(ct_app_env$tradeDiag(result),
                      info = paste("trade diagnostics row", i))
      ct_expect_no_error(ct_app_env$tradeDiag(result, mktElast = TRUE),
                      info = paste("trade elasticity diagnostics row", i))
      ct_expect_no_error(ct_app_env$tradeNoPurch(result),
                      info = paste("trade details row", i))
      if (row$page == "Quotas") {
        quota_details <- ct_app_env$tradeNoPurch(result)
        expect_identical(rownames(quota_details), c("Current Quota", "New Quota"))
      }
    }
  }
})

test_that("EXTENDED/NIGHTLY UI rows match independent public legacy constructors", {
  testthat::skip_if_not(ct_extended_tier(),
                        "set COMPETITIONTOOLBOX_TEST_TIER=EXTENDED or NIGHTLY")
  ct_load_core()
  registry <- ct_app_env$model_registry()

  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]
    data <- ct_default_inputs(row$page)
    if (row$demand_id == "aids") {
      migrated <- ct_with_seed(260911L, function() ct_run_row(row, data))
      legacy <- ct_with_seed(260911L, function() ct_legacy_model(row, data))
    } else {
      migrated <- ct_run_row(row, data)
      legacy <- ct_legacy_model(row, data)
    }

    calc <- switch(
      row$upstream_package,
      antitrust = antitrust::calcPrices,
      trade = trade::calcPrices,
      vertical = vertical::calcPrices
    )
    shares <- switch(
      row$upstream_package,
      antitrust = antitrust::calcShares,
      trade = trade::calcShares,
      vertical = vertical::calcShares
    )
    share_args <- if (row$demand_id %in% c("ces", "aids")) {
      list(revenue = TRUE)
    } else {
      list()
    }

    expect_equal(
      ct_unname_recursive(do.call(calc, list(migrated, preMerger = FALSE))),
      ct_unname_recursive(do.call(calc, list(legacy, preMerger = FALSE))),
      tolerance = if (row$page == "Vertical") 1e-7 else 1e-8,
      info = paste("post prices row", i)
    )
    expect_equal(
      ct_unname_recursive(do.call(shares, c(
        list(migrated, preMerger = FALSE), share_args
      ))),
      ct_unname_recursive(do.call(shares, c(
        list(legacy, preMerger = FALSE), share_args
      ))),
      tolerance = if (row$page == "Vertical") 1e-7 else 1e-8,
      info = paste("post shares row", i)
    )

    ## Margins and quantities are useful additional parity checks for models
    ## where prices alone do not identify the equilibrium (notably Cournot).
    calc_margins <- switch(
      row$upstream_package,
      antitrust = antitrust::calcMargins,
      trade = trade::calcMargins,
      vertical = vertical::calcMargins
    )
    calc_quantities <- switch(
      row$upstream_package,
      antitrust = antitrust::calcQuantities,
      trade = trade::calcQuantities,
      vertical = vertical::calcQuantities
    )
    expect_equal(
      ct_unname_recursive(calc_margins(migrated, preMerger = FALSE)),
      ct_unname_recursive(calc_margins(legacy, preMerger = FALSE)),
      tolerance = if (row$page == "Vertical") 1e-7 else 1e-8,
      info = paste("post margins row", i)
    )
    expect_equal(
      ct_unname_recursive(calc_quantities(migrated, preMerger = FALSE)),
      ct_unname_recursive(calc_quantities(legacy, preMerger = FALSE)),
      tolerance = if (row$page == "Vertical") 1e-7 else 1e-8,
      info = paste("post quantities row", i)
    )
  }
})

test_that("route metadata records the public lifecycle fit and observed inputs", {
  ct_load_core()
  cases <- list(
    list(page = "Horizontal", supply = "Bertrand", demand = "logit"),
    list(page = "Vertical", supply = "Bertrand", demand = "logit"),
    list(page = "Tariffs", supply = "Bertrand", demand = "logit"),
    list(page = "Quotas", supply = "Bertrand", demand = "logit")
  )

  for (case in cases) {
    row <- ct_app_env$model_registry()
    row <- row[row$page == case$page & row$supply == case$supply &
                 row$demand_label == case$demand, , drop = FALSE]
    expect_equal(nrow(row), 1L)
    result <- ct_run_row(row, ct_default_inputs(case$page))
    fit <- attr(result, "ct_fit", exact = TRUE)

    expect_s4_class(fit, ct_fit_class(row))
    expect_true(methods::is(fit, "StructuralFit"))
    expect_true(isS4(methods::slot(fit, "model")))
    expect_true(is.list(methods::slot(fit, "parameters")))
    expect_true(is.list(methods::slot(fit, "observed")))
    expect_true(is.list(methods::slot(fit, "diagnostics")))
    ct_same_spec(methods::slot(fit, "spec"),
                 attr(result, "ct_model_spec", exact = TRUE)$upstream_spec)

    if (case$page == "Horizontal") {
      expect_true(all(c("Prices", "Output", "Margins", "mcDelta") %in%
                        names(attr(result, "ct_model_inputs", exact = TRUE))))
    }
    if (case$page %in% c("Tariffs", "Quotas")) {
      expect_true(all(c("Prices", "Output", "Margins", "policyPre", "policyPost") %in%
                        names(attr(result, "ct_model_inputs", exact = TRUE))))
    }
  }
})

test_that("antitrust route preserves an independently anchored price/share outcome", {
  ct_load_core()
  data <- ct_complete_data(ct_default_inputs("Horizontal"))
  result <- ct_app_env$mergersSims(
    "Bertrand", "logit", data, mktElast = -1, type = "Horizontal"
  )

  ## This is the public legacy constructor used as the behavioral oracle.  It
  ## is deliberately independent of competitiontoolbox's route dispatcher.
  direct <- suppressWarnings(antitrust::logit.alm(
    prices = data[[grep("^Prices", names(data), value = TRUE)]],
    shares = data$Quantities / sum(data$Quantities),
    margins = data[[grep("^Margins", names(data), value = TRUE)]],
    ownerPre = data[[grep("^Pre-merger", names(data), value = TRUE)]],
    ownerPost = data[[grep("^Post-merger", names(data), value = TRUE)[1L]]],
    mktElast = -1,
    insideSize = sum(data$Quantities),
    mcDelta = rep(0, nrow(data))
  ))

  expect_equal(ct_numeric(antitrust::calcPrices(result, preMerger = FALSE)),
               c(10.7275782286253, 11.0216125260618,
                 10.1621583741652, 10.1621583741652),
               tolerance = 1e-8)
  expect_equal(
    ct_numeric(antitrust::calcPrices(result, preMerger = FALSE)),
    ct_numeric(antitrust::calcPrices(direct, preMerger = FALSE)),
    tolerance = 1e-8
  )
  expect_equal(
    ct_numeric(antitrust::calcShares(result, preMerger = FALSE)),
    c(.286269634172035, .182807654101367, .195006845930099,
      .0975034229650493),
    tolerance = 1e-8
  )
  expect_equal(
    ct_numeric(antitrust::calcShares(result, preMerger = FALSE)),
    ct_numeric(antitrust::calcShares(direct, preMerger = FALSE)),
    tolerance = 1e-8
  )
})

test_that("trade tariff and quota routes preserve public legacy outcomes", {
  ct_load_core()
  expected <- list(
    Tariffs = list(
      prices = c(11.5946250183838, 11.7710021308224,
                 10.3254018658481, 10.3254018658481),
      shares = c(.220313243753889, .149561750307568,
                 .225666116282837, .112833058141418)
    ),
    Quotas = list(
      prices = c(11.1549327478338, 11.1549327518375,
                 10.2260434778106, 10.2260434770576),
      shares = c(.24689881373472, .185174109882143,
                 .208653644505554, .104326822297166)
    )
  )

  for (page in names(expected)) {
    data <- ct_default_inputs(page)
    normalized <- ct_app_env$normalize_model_inputs(page, data)
    result <- ct_app_env$tradeSims(
      "Bertrand", "logit", data, mktElast = -1, type = page
    )

    if (page == "Tariffs") {
      direct <- suppressWarnings(trade::bertrand_tariff(
        demand = "logit", prices = normalized$Prices,
        quantities = normalized$Output, margins = normalized$Margins,
        owner = normalized$Owner, mktElast = -1,
        tariffPre = normalized$policyPre, tariffPost = normalized$policyPost
      ))
    } else {
      direct <- suppressWarnings(trade::bertrand_quota(
        demand = "logit", prices = normalized$Prices,
        quantities = normalized$Output, margins = normalized$Margins,
        owner = normalized$Owner, mktElast = -1,
        quotaPre = normalized$policyPre, quotaPost = normalized$policyPost
      ))
    }

    expect_equal(ct_numeric(trade::calcPrices(result, preMerger = FALSE)),
                 expected[[page]]$prices, tolerance = 1e-8,
                 info = paste(page, "anchored prices"))
    expect_equal(ct_numeric(trade::calcShares(result, preMerger = FALSE)),
                 expected[[page]]$shares, tolerance = 1e-8,
                 info = paste(page, "anchored shares"))
    expect_equal(ct_numeric(trade::calcPrices(result, preMerger = FALSE)),
                 ct_numeric(trade::calcPrices(direct, preMerger = FALSE)),
                 tolerance = 1e-8, info = paste(page, "legacy price parity"))
    expect_equal(ct_numeric(trade::calcShares(result, preMerger = FALSE)),
                 ct_numeric(trade::calcShares(direct, preMerger = FALSE)),
                 tolerance = 1e-8, info = paste(page, "legacy share parity"))
  }
})

test_that("AIDS benchmark and Logit-Cournot tariff route satisfy economics", {
  ct_load_core()
  data <- ct_complete_data(ct_default_inputs("Tariffs"))
  normalized <- ct_app_env$normalize_model_inputs("Tariffs", data)

  aids <- ct_app_env$tradeSims(
    "Bertrand", "aids", data, mktElast = -1, type = "Tariffs"
  )
  capture.output({ aids_direct <- suppressWarnings(
    trade::bertrand_tariff(
      demand = "aids", prices = normalized$Prices,
      quantities = normalized$Output, margins = normalized$Margins,
      owner = normalized$Owner, mktElast = -1,
      tariffPre = normalized$policyPre, tariffPost = normalized$policyPost
    )
  ) })
  expect_equal(ct_numeric(trade::calcPrices(aids, preMerger = FALSE)),
               c(12.1541096267193, 12.1802629353111,
                 10.8644048365191, 10.8644047842231),
               tolerance = 1e-8)
  expect_equal(ct_numeric(trade::calcPrices(aids, preMerger = FALSE)),
               ct_numeric(trade::calcPrices(aids_direct, preMerger = FALSE)),
               tolerance = 1e-8)
  expect_identical(attr(aids, "ct_simulation_api", exact = TRUE),
                   "trade::simulate")
  expect_s4_class(attr(aids, "ct_fit", exact = TRUE), "TradeFit")

  registry <- ct_app_env$model_registry()
  row <- registry[registry$page == "Tariffs" &
                    registry$supply == "Cournot" &
                    registry$demand_label == "logit" &
                    registry$calibration == "known", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  ## The known row uses the ALM Logit-Cournot calibration, just as the
  ## corresponding antitrust row does.
  expect_identical(row$variant, "alm")
  cournot <- ct_app_env$tradeSims(
    "Cournot", "logit", data, mktElast = -1, type = "Tariffs"
  )
  capture.output({ cournot_direct <- suppressWarnings(
    trade::logit_cournot_tariff(
      prices = normalized$Prices, quantities = normalized$Output,
      shares = normalized$Output / sum(normalized$Output),
      margins = normalized$Margins, owner = as.character(normalized$Owner),
      mktElast = -1, calibration = "alm",
      tariffPre = normalized$policyPre, tariffPost = normalized$policyPost,
      labels = normalized$Name
    )
  ) })
  ## Migration parity: A (permanent).
  ## This catches a unique economic failure: treating a tariff as an
  ## effective-cost change instead of retained revenue in a multiproduct
  ## Cournot firm's physical-profit FOCs.
  alpha <- cournot@slopes$alpha
  share_pre <- cournot@shares * cournot@shareInside
  outside_pre <- 1 - sum(share_pre)
  mean_utility <- log(share_pre / outside_pre) - alpha * cournot@prices

  share_post <- ct_numeric(trade::calcShares(cournot, preMerger = FALSE))
  outside_post <- 1 - sum(share_post)
  price_inverse <- (log(share_post / outside_post) - mean_utility) / alpha
  expect_equal(price_inverse,
               ct_numeric(trade::calcPrices(cournot, preMerger = FALSE)),
               tolerance = 1e-8)

  market_size <- cournot@insideSize / cournot@shareInside
  quantity_post <- market_size * share_post
  outside_quantity <- market_size * outside_post
  retention_pre <- 1 - cournot@tariffPre
  retention_post <- 1 - cournot@tariffPost
  physical_cost <- retention_pre * cournot@mcPre
  owner <- as.character(normalized$Owner)
  physical_profit_foc <- vapply(seq_along(price_inverse), function(product) {
    same_firm <- owner == owner[[product]]
    retention_post[[product]] * price_inverse[[product]] -
      physical_cost[[product]] + retention_post[[product]] / alpha +
      sum(retention_post[same_firm] * quantity_post[same_firm]) /
        (alpha * outside_quantity)
  }, numeric(1))
  expect_equal(physical_profit_foc, rep(0, length(physical_profit_foc)),
               tolerance = 1e-5)
  expect_equal(ct_numeric(trade::calcPrices(cournot, preMerger = FALSE)),
               ct_numeric(trade::calcPrices(cournot_direct, preMerger = FALSE)),
               tolerance = 1e-8)
})

test_that("vertical route preserves the public legacy bargaining outcome", {
  ct_load_core()
  data <- ct_complete_data(ct_default_inputs("Vertical"))
  result <- ct_app_env$mergersSims(
    "Bertrand", "logit", data, mktElast = -1, type = "Vertical"
  )
  direct <- suppressWarnings(vertical::vertical.barg(
    supplyDown = "bertrand",
    sharesDown = data$sharesDown,
    pricesDown = data$pricesDown,
    marginsDown = data$marginsDown,
    ownerPreDown = data$ownerPreDown,
    ownerPostDown = data$ownerPostDown,
    pricesUp = data$pricesUp,
    marginsUp = data$marginsUp,
    ownerPreUp = data$ownerPreUp,
    ownerPostUp = data$ownerPostUp,
    mcDeltaDown = rep(0, nrow(data)),
    mcDeltaUp = rep(0, nrow(data)),
    priceOutside = 0,
    labels = data$Name
  ))

  for (pre in c(TRUE, FALSE)) {
    expect_equal(
      ct_unname_recursive(vertical::calcPrices(result, preMerger = pre)),
      ct_unname_recursive(vertical::calcPrices(direct, preMerger = pre)),
      tolerance = 1e-7, info = paste("prices preMerger", pre)
    )
    expect_equal(
      ct_unname_recursive(vertical::calcShares(result, preMerger = pre)),
      ct_unname_recursive(vertical::calcShares(direct, preMerger = pre)),
      tolerance = 1e-7, info = paste("shares preMerger", pre)
    )
  }
})

test_that("standalone generated code parses and representative snippets execute", {
  ct_load_core(include_templates = TRUE)
  ct_app_env$nPossProds <- 4L
  input_data <- list(
    Horizontal = ct_default_inputs("Horizontal"),
    Vertical = ct_default_inputs("Vertical"),
    Tariffs = ct_default_inputs("Tariffs"),
    Quotas = ct_default_inputs("Quotas")
  )

  ## Template functions read these values from their application environment,
  ## while the generated program itself is evaluated in a fresh environment.
  ct_app_env$values <- list(inputData = input_data$Horizontal)
  ct_app_env$valuesVertical <- list(inputData = input_data$Vertical)
  ct_app_env$valuesTariffs <- list(inputData = input_data$Tariffs)
  ct_app_env$valuesQuota <- list(inputData = input_data$Quotas)
  ct_app_env$input <- list(
    calcElast = "market elasticity", enterElast = -1,
    calcElastTariffs = "market elasticity", enterElastTariffs = -1,
    calcElastQuota = "market elasticity", enterElastQuota = -1
  )
  ct_app_env$supply <- function() "Bertrand"
  ct_app_env$demand <- function() "logit"

  for (page in names(input_data)) {
    code <- if (page %in% c("Horizontal", "Vertical")) {
      ct_app_env$mergersTemplateCode(page)
    } else {
      ct_app_env$tradeTemplateCode(page)
    }
    program <- paste(code, collapse = "\n")
    expect_silent(parse(text = program))

    eval_env <- new.env(parent = ct_app_env)
    ct_expect_no_error(
      capture.output(suppressWarnings(eval(parse(text = program), envir = eval_env))),
      info = paste("generated", page, "program")
    )
  }
})

test_that("price-free AIDS reporting and generated code remain usable", {
  ct_load_core(include_templates = TRUE)
  ct_app_env$nPossProds <- 10L
  horizontal <- ct_default_inputs("Horizontal")
  tariffs <- ct_default_inputs("Tariffs")
  horizontal[[grep("^Prices", names(horizontal), value = TRUE)[1L]]] <- NA_real_
  tariffs[[grep("^Prices", names(tariffs), value = TRUE)[1L]]] <- NA_real_

  ct_app_env$supply <- function() "Bertrand"
  ct_app_env$demand <- function() "aids"
  ct_app_env$input <- list(
    calcElast = "known elasticity", enterElast = -1,
    calcElastTariffs = "known elasticity", enterElastTariffs = -1
  )

  ct_app_env$values <- list(inputData = horizontal)
  horizontal_program <- paste(
    ct_app_env$mergersTemplateCode("Horizontal"), collapse = "\n"
  )
  expect_silent(parse(text = horizontal_program))
  ct_expect_no_error(capture.output(suppressWarnings(eval(
    parse(text = horizontal_program), envir = new.env(parent = ct_app_env)
  ))))

  tariff_result <- suppressWarnings(ct_app_env$tradeSims(
    "Bertrand", "aids", tariffs, mktElast = -1, type = "Tariffs"
  ))
  report <- ct_app_env$tradeMarketReport(tariff_result)
  expect_identical(
    names(report),
    c(
      "Domestic Firm Price Change (%)",
      "Foreign Firm Price Change (%)",
      "Industry Price Change (%)"
    )
  )
  expect_true(all(is.finite(unlist(report, use.names = FALSE))))

  ct_app_env$valuesTariffs <- list(inputData = tariffs)
  trade_program <- paste(
    ct_app_env$tradeTemplateCode("Tariffs"), collapse = "\n"
  )
  expect_silent(parse(text = trade_program))
  ct_expect_no_error(capture.output(suppressWarnings(eval(
    parse(text = trade_program), envir = new.env(parent = ct_app_env)
  ))))
})
