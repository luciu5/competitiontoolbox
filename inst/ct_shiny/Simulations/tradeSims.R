tradeSims <- function(supply, demand, indata, mktElast, type = c("Tariffs", "Quotas")) {
  type <- match.arg(type)
  spec <- model_spec(type, supply, demand)
  run_model(spec, indata, list(mktElast = mktElast))
}

## Trade routes deliberately go through trade's public fit/simulation
## lifecycle.  In particular, tariff/quota post-state is never passed to
## calibrate(): this keeps a reusable calibrated baseline in the returned fit.
run_tariff_model <- function(spec, indata, params = list()) {
  if (!identical(spec$page, "Tariffs")) {
    stop("run_tariff_model() requires a tariff model specification.", call. = FALSE)
  }

  known_elast <- identical(spec$calibration, "known")
  mkt_elast <- params$mktElast
  if (is.null(mkt_elast)) mkt_elast <- NA_real_

  first_price <- which(!is.na(indata$Prices))[1]
  is_cournot_matrix <- identical(spec$conduct, "cournot") &&
    !identical(spec$demand_id, "logit")

  if (is_cournot_matrix) {
    ## The legacy Cournot tariff UI is a plant-by-one-product market.  Keep
    ## its matrix shapes and the first observed price/product label exactly.
    calibration_args <- list(
      prices = indata$Prices[first_price],
      quantities = as.matrix(indata$Output),
      margins = as.matrix(indata$Margins),
      owner = as.character(indata$Owner),
      tariffPre = as.matrix(indata$policyPre),
      labels = list(as.character(indata$Name), as.character(indata$Name[first_price]))
    )
  } else {
    calibration_args <- list(
      prices = indata$Prices,
      quantities = indata$Output,
      margins = indata$Margins,
      labels = as.character(indata$Name),
      tariffPre = indata$policyPre
    )
    if (!identical(spec$conduct, "moncom")) {
      calibration_args$owner <- as.character(indata$Owner)
    }
  }

  ## Unknown-elasticity rows intentionally omit mktElast.  Supplying an
  ## explicit NA is not equivalent for all legacy calibrators (notably the
  ## Logit-Cournot route), because argument presence selects calibration.
  if (known_elast) calibration_args$mktElast <- mkt_elast

  fit <- do.call(
    trade::calibrate,
    c(list(demand = spec$upstream_spec), calibration_args)
  )
  ## The frozen upstream lifecycle currently leaves the TariffAIDS priceDelta at
  ## its calibration value: its simulate() path recalculates marginal costs
  ## but does not call the public calcPriceDelta() method before calcPrices().
  ## Keep the lifecycle fit, then use the public legacy constructor for this
  ## one affected counterfactual so the established AIDS economics remain
  ## unchanged.  No private helpers or slots are accessed here.
  if (identical(spec$demand_id, "aids")) {
    legacy_args <- calibration_args
    legacy_args$tariffPost <- indata$policyPost
    result <- do.call(
      trade::bertrand_tariff,
      c(list(demand = "aids"), legacy_args)
    )
    simulation_api <- "trade::bertrand_tariff (TariffAIDS compatibility)"
  } else if (is_cournot_matrix) {
    result <- trade::simulate(fit, tariffPost = as.matrix(indata$policyPost))
    simulation_api <- "trade::simulate"
  } else {
    result <- trade::simulate(fit, tariffPost = indata$policyPost)
    simulation_api <- "trade::simulate"
  }

  ## mktElast is a model primitive rather than a row-level input.  Retain it
  ## for the diagnostics tab without reading the trade S4 object's slots.
  attr(result, "ct_mktElast") <- if (known_elast) mkt_elast else NA_real_
  list(fit = fit, result = result, simulation_api = simulation_api)
}

run_quota_model <- function(spec, indata, params = list()) {
  if (!identical(spec$page, "Quotas")) {
    stop("run_quota_model() requires a quota model specification.", call. = FALSE)
  }

  calibration_args <- list(
    prices = indata$Prices,
    quantities = indata$Output,
    margins = indata$Margins,
    owner = as.character(indata$Owner),
    labels = as.character(indata$Name),
    quotaPre = indata$policyPre
  )
  if (identical(spec$calibration, "known")) {
    calibration_args$mktElast <- if (is.null(params$mktElast)) NA_real_ else params$mktElast
  }

  fit <- do.call(
    trade::calibrate,
    c(list(demand = spec$upstream_spec), calibration_args)
  )
  result <- trade::simulate(fit, quotaPost = indata$policyPost)
  attr(result, "ct_mktElast") <- if (identical(spec$calibration, "known")) {
    if (is.null(params$mktElast)) NA_real_ else params$mktElast
  } else {
    NA_real_
  }
  list(fit = fit, result = result, simulation_api = "trade::simulate")
}
