## Test helpers for the Shiny model routes.
##
## The application is intentionally kept in inst/ct_shiny rather than in the
## package namespace.  Source the application files into one private
## environment so that tests exercise the same functions without attaching
## any of the upstream packages (whose exports overlap).

ct_app_file <- function(...) {
  parts <- list(...)
  ## `ct_load_core()` passes each two-component path as one list element.
  if (length(parts) == 1L && length(parts[[1L]]) > 1L) {
    parts <- as.list(parts[[1L]])
  }
  relative <- do.call(file.path, parts)
  candidates <- c(
    file.path("inst", "ct_shiny", relative),
    file.path("..", "..", "inst", "ct_shiny", relative)
  )
  for (path in candidates) {
    if (file.exists(path)) return(path)
  }
  installed <- system.file("ct_shiny", relative, package = "competitiontoolbox")
  if (nzchar(installed) && file.exists(installed)) return(installed)
  stop("Cannot find competitiontoolbox application file: ", relative,
       call. = FALSE)
}

ct_app_env <- new.env(parent = globalenv())

ct_source_app <- function(...) {
  source(ct_app_file(...), local = ct_app_env)
  invisible(ct_app_env)
}

ct_require_stack <- function() {
  testthat::skip_if_not_installed("antitrust")
  testthat::skip_if_not_installed("trade")
  testthat::skip_if_not_installed("vertical")

  ## These are the public lifecycle APIs used by the refactored routes.  An
  ## older user library may contain the legacy packages but not the lifecycle
  ## release required by this package's DESCRIPTION.
  for (pkg in c("antitrust", "trade", "vertical")) {
    if (pkg == "antitrust" && utils::packageVersion(pkg) < "0.99.39") {
      testthat::skip("antitrust >= 0.99.39 is required for lifecycle tests")
    }
    if (pkg == "trade" && utils::packageVersion(pkg) < "0.8.6") {
      testthat::skip("trade >= 0.8.6 is required for lifecycle tests")
    }
    if (pkg == "vertical" && utils::packageVersion(pkg) < "0.1.0") {
      testthat::skip("vertical >= 0.1.0 is required for lifecycle tests")
    }
  }

}

ct_load_core <- function(include_templates = FALSE) {
  ct_require_stack()
  files <- list(
    c("R", "modelRegistry.R"),
    c("Inputs", "mergersInputs.R"),
    c("Inputs", "tradeInputs.R"),
    c("Simulations", "mergersSims.R"),
    c("Simulations", "tradeSims.R"),
    c("Summary", "mergersSummary.R"),
    c("Summary", "tradeMarketReport.R"),
    c("Details", "mergersNoPurch.R"),
    c("Details", "tradeNoPurch.R"),
    c("Diagnostics", "mergersDiag.R"),
    c("Diagnostics", "tradeDiag.R")
  )
  if (isTRUE(include_templates)) {
    files <- c(files, list(
      c("R", "mergersTemplateCode.R"),
      c("R", "tradeTemplateCode.R")
    ))
  }
  for (path in files) ct_source_app(path)
  invisible(ct_app_env)
}

ct_default_inputs <- function(page) {
  ct_load_core()
  ## Ten rows is the application's default grid; route normalization then
  ## drops the unused rows.  Keeping that shape here tests the real UI input
  ## path rather than a hand-trimmed data frame.
  n <- 10L
  ## The horizontal input generator historically reads this value from its
  ## enclosing application environment rather than its `nrows` argument.
  ct_app_env$nPossProds <- n
  switch(
    page,
    Horizontal = ct_app_env$mergersInputs(n, type = "Horizontal"),
    Vertical = ct_app_env$mergersInputs(n, type = "Vertical"),
    Tariffs = ct_app_env$tradeInputs(n, type = "Tariffs"),
    Quotas = ct_app_env$tradeInputs(n, type = "Quotas"),
    stop("Unsupported test page: ", page, call. = FALSE)
  )
}

ct_run_row <- function(row, data = ct_default_inputs(row$page)) {
  ct_load_core()
  if (row$page %in% c("Horizontal", "Vertical")) {
    ct_app_env$mergersSims(
      row$supply, row$demand_label, data, mktElast = -1,
      type = row$page
    )
  } else {
    ct_app_env$tradeSims(
      row$supply, row$demand_label, data, mktElast = -1,
      type = row$page
    )
  }
}

ct_fit_class <- function(row) {
  switch(
    row$upstream_package,
    antitrust = "AntitrustFit",
    vertical = "VerticalFit",
    trade = "TradeFit",
    stop("Unsupported upstream package: ", row$upstream_package,
         call. = FALSE)
  )
}

ct_upstream_spec <- function(row) {
  switch(
    row$upstream_package,
    antitrust = antitrust::model_spec(
      demand = row$demand_id,
      conduct = row$conduct,
      variant = row$variant
    ),
    vertical = vertical::model_spec(
      demand = row$demand_id,
      conduct = row$conduct,
      variant = row$variant
    ),
    trade = trade::model_spec(
      demand = row$demand_id,
      conduct = row$conduct,
      variant = row$variant,
      policy = row$policy
    ),
    stop("Unsupported upstream package: ", row$upstream_package,
         call. = FALSE)
  )
}

ct_same_spec <- function(actual, expected) {
  ## `expect_equal()` compares class and names too, while the explicit field
  ## check gives a useful failure if a route silently drops a model primitive.
  testthat::expect_equal(actual, expected)
  testthat::expect_equal(actual$demand, expected$demand)
  testthat::expect_equal(actual$conduct, expected$conduct)
  testthat::expect_equal(actual$variant, expected$variant)
  if (!is.null(expected$policy)) {
    testthat::expect_equal(actual$policy, expected$policy)
  }
  invisible(TRUE)
}

ct_numeric <- function(x) unname(as.numeric(x))

ct_complete_data <- function(data) {
  if ("Name" %in% names(data)) {
    data <- data[!is.na(data$Name) & nzchar(data$Name), , drop = FALSE]
  }
  data
}

ct_unname_recursive <- function(x) {
  if (is.list(x)) return(lapply(x, ct_unname_recursive))
  if (is.numeric(x)) return(unname(x))
  x
}

ct_expect_no_error <- function(expr, info = NULL) {
  ## testthat 3's expect_no_error() intentionally has no `info` argument.
  ## Keep contextual diagnostics for the row-wise integration checks by
  ## expressing the same assertion through expect_error().
  testthat::expect_error(force(expr), NA, info = info)
}

ct_test_tier <- function() {
  toupper(Sys.getenv("COMPETITIONTOOLBOX_TEST_TIER", unset = "FAST"))
}

ct_extended_tier <- function() {
  ct_test_tier() %in% c("EXTENDED", "NIGHTLY")
}

ct_with_seed <- function(seed, fun) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(fun())
}

ct_silent_legacy <- function(fun) {
  value <- NULL
  capture.output(value <- suppressWarnings(fun()))
  value
}

ct_legacy_model <- function(row, data) {
  ## Shape arguments from the raw UI table here, independently of
  ## normalize_model_inputs()/run_model().  This is intentionally a test-only
  ## legacy oracle: it calls the public constructors whose equations predate
  ## the migrated route.
  data <- ct_complete_data(data)

  if (row$page == "Horizontal") {
    prices <- as.numeric(data[[grep("^Prices", names(data), value = TRUE)]])
    output <- as.numeric(data[[grep("Quantities|Revenues", names(data),
                                     value = TRUE)]])
    margins <- as.numeric(data[[grep("^Margins", names(data), value = TRUE)]])
    owner_pre <- as.character(data[[grep("^Pre-merger", names(data),
                                         value = TRUE)]])
    owner_post <- as.character(data[[grep("^Post-merger", names(data),
                                          value = TRUE)[1L]]])
    cost_name <- grep("Cost Changes", names(data), value = TRUE)
    mc_delta <- if (length(cost_name)) as.numeric(data[[cost_name[1L]]]) else
      rep(0, length(prices))
    shares_quantity <- output / sum(output)
    shares_revenue <- prices * shares_quantity / sum(prices * shares_quantity)
    owner_pre_matrix <- 1 * outer(owner_pre, owner_pre, "==")
    owner_post_matrix <- 1 * outer(owner_post, owner_post, "==")
    inside_size <- if (row$demand_id %in% c("ces", "aids")) {
      sum(prices * output)
    } else {
      sum(output)
    }

    if (row$conduct == "bertrand" && row$demand_id == "logit") {
      return(ct_silent_legacy(function() antitrust::logit.alm(
        prices = prices, shares = shares_quantity, margins = margins,
        ownerPre = owner_pre, ownerPost = owner_post,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        insideSize = inside_size, mcDelta = mc_delta
      )))
    }
    if (row$conduct == "bertrand" && row$demand_id == "ces") {
      return(ct_silent_legacy(function() antitrust::ces.alm(
        prices = prices, shares = shares_revenue, margins = margins,
        ownerPre = owner_pre, ownerPost = owner_post,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        insideSize = inside_size, mcDelta = mc_delta
      )))
    }
    if (row$conduct == "bertrand" && row$demand_id == "aids") {
      return(ct_silent_legacy(function() antitrust::aids(
        prices = prices, shares = shares_revenue, margins = margins,
        ownerPre = owner_pre, ownerPost = owner_post,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        insideSize = inside_size, mcDelta = mc_delta
      )))
    }
    if (row$conduct == "cournot" && row$demand_id == "logit") {
      return(ct_silent_legacy(function() antitrust::logit.cournot.alm(
        prices = prices, shares = shares_quantity, margins = margins,
        ownerPre = owner_pre, ownerPost = owner_post,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        insideSize = inside_size, mcDelta = mc_delta
      )))
    }
    if (row$conduct == "cournot" && row$demand_id %in% c("linear", "loglin")) {
      first_price <- prices[which(!is.na(prices))[1L]]
      demand <- if (row$demand_id == "loglin") "log" else "linear"
      return(ct_silent_legacy(function() antitrust::cournot(
        prices = first_price, quantities = as.matrix(output),
        margins = as.matrix(margins), demand = demand,
        ownerPre = owner_pre_matrix, ownerPost = owner_post_matrix,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        mcDelta = mc_delta,
        labels = list(as.character(data$Name), as.character(data$Name[1L]))
      )))
    }
    if (row$conduct == "auction2nd") {
      return(ct_silent_legacy(function() antitrust::auction2nd.logit.alm(
        prices = prices, shares = shares_quantity, margins = margins * prices,
        ownerPre = owner_pre, ownerPost = owner_post,
        mktElast = if (row$calibration == "known") -1 else NA_real_,
        insideSize = inside_size, mcDelta = mc_delta
      )))
    }
    stop("unshaped antitrust legacy row: ", row$demand_id, "/", row$conduct,
         call. = FALSE)
  }

  if (row$page == "Vertical") {
    return(ct_silent_legacy(function() vertical::vertical.barg(
      supplyDown = if (row$variant == "auction2nd") "2nd" else "bertrand",
      sharesDown = as.numeric(data$sharesDown),
      pricesDown = as.numeric(data$pricesDown),
      marginsDown = as.numeric(data$marginsDown),
      ownerPreDown = as.character(data$ownerPreDown),
      ownerPostDown = as.character(data$ownerPostDown),
      pricesUp = as.numeric(data$pricesUp),
      marginsUp = as.numeric(data$marginsUp),
      ownerPreUp = as.character(data$ownerPreUp),
      ownerPostUp = as.character(data$ownerPostUp),
      mcDeltaDown = rep(0, nrow(data)), mcDeltaUp = rep(0, nrow(data)),
      priceOutside = 0, labels = as.character(data$Name)
    )))
  }

  prices <- as.numeric(data[[grep("^Prices", names(data), value = TRUE)]])
  output <- as.numeric(data[[grep("Quantities|Revenues", names(data),
                                   value = TRUE)]])
  margins <- as.numeric(data[[grep("^Margins", names(data), value = TRUE)]])
  owner <- as.character(data[[grep("^Owner$", names(data), value = TRUE)]])
  pre_name <- grep("^Current", names(data), value = TRUE)
  post_name <- grep("^New", names(data), value = TRUE)
  policy_pre <- as.numeric(data[[pre_name[1L]]])
  policy_post <- as.numeric(data[[post_name[1L]]])
  known <- identical(row$calibration, "known")
  mkt <- if (known) list(mktElast = -1) else list()

  if (row$page == "Quotas") {
    return(ct_silent_legacy(function() do.call(trade::bertrand_quota, c(
      list(demand = row$demand_id, prices = prices, quantities = output,
           margins = margins, owner = owner,
           quotaPre = policy_pre, quotaPost = policy_post), mkt)
    )))
  }
  if (row$conduct == "bertrand") {
    return(ct_silent_legacy(function() do.call(trade::bertrand_tariff, c(
      list(demand = row$demand_id, prices = prices, quantities = output,
           margins = margins, owner = owner,
           tariffPre = policy_pre, tariffPost = policy_post), mkt)
    )))
  }
  if (row$conduct == "moncom") {
    return(ct_silent_legacy(function() do.call(
      trade::monopolistic_competition_tariff,
      c(list(demand = row$demand_id, prices = prices, quantities = output,
             margins = margins, mktSize = sum(output),
             tariffPre = policy_pre, tariffPost = policy_post), mkt)
    )))
  }
  if (row$conduct == "cournot" && row$demand_id == "logit") {
    return(ct_silent_legacy(function() do.call(trade::logit_cournot_tariff, c(
      list(prices = prices, quantities = output, shares = output / sum(output),
           margins = margins, owner = owner,
           calibration = if (row$variant == "alm") "alm" else "diversion",
           tariffPre = policy_pre, tariffPost = policy_post,
           labels = as.character(data$Name)), mkt)
    )))
  }
  if (row$conduct == "cournot" && row$demand_id %in% c("linear", "loglin")) {
    first_price <- prices[which(!is.na(prices))[1L]]
    demand <- if (row$demand_id == "loglin") "log" else "linear"
    return(ct_silent_legacy(function() do.call(trade::cournot_tariff, c(
      list(prices = first_price, quantities = as.matrix(output),
           margins = as.matrix(margins), demand = demand, owner = owner,
           tariffPre = as.matrix(policy_pre), tariffPost = as.matrix(policy_post),
           labels = list(as.character(data$Name), as.character(data$Name[1L]))), mkt)
    )))
  }
  stop("unshaped trade legacy row: ", row$demand_id, "/", row$conduct,
       call. = FALSE)
}
