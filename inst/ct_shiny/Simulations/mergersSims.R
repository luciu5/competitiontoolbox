mergersSims <- function(supply, demand = "logit", indata, mktElast = NA_real_,
                        type = c("Horizontal", "Vertical")) {
  type <- match.arg(type)
  spec <- model_spec(type, supply, demand)
  run_model(spec, indata, list(mktElast = mktElast))
}

## The Shiny registry owns the page-level dispatch, while these adapters own
## the translation from the app's tabular inputs to each upstream package's
## public lifecycle.  The fitted object is deliberately returned alongside
## the one-step legacy result; run_model() attaches the app metadata to the
## latter after this function returns.
run_horizontal_model <- function(spec, indata, params = list()) {
  prices <- as.numeric(indata$Prices)
  output <- as.numeric(indata$Output)
  margins <- as.numeric(indata$Margins)

  miss_prices <- anyNA(prices)
  shares_quantity <- output / sum(output, na.rm = TRUE)
  shares_revenue <- shares_quantity
  inside_size <- sum(output, na.rm = TRUE)

  if (!miss_prices) {
    if (spec$demand_id %in% c("ces", "aids")) {
      inside_size <- sum(prices * output, na.rm = TRUE)
    }
    shares_revenue <- prices * shares_quantity /
      sum(prices * shares_quantity, na.rm = TRUE)
    if (identical(spec$supply, "2nd Score Auction")) {
      margins <- margins * prices
    }
  }

  ## The app records one owner label per product.  Cournot's homogeneous
  ## product route interprets these as plant owners, so the same ownership
  ## matrices work for both differentiated and homogeneous-product models.
  owner_pre_labels <- as.character(indata[["Pre-merger\n Owner"]])
  owner_post_labels <- as.character(indata[["Post-merger\n Owner"]])
  owner_pre <- 1 * outer(owner_pre_labels, owner_pre_labels, "==")
  owner_post <- 1 * outer(owner_post_labels, owner_post_labels, "==")

  known_elast <- identical(spec$calibration, "known")
  mkt_elast <- params$mktElast
  if (is.null(mkt_elast)) mkt_elast <- NA_real_
  if (!known_elast) mkt_elast <- NA_real_
  include_mkt_elast <- known_elast || identical(spec$conduct, "cournot")

  fit_args <- list(
    demand = spec$upstream_spec,
    prices = prices,
    shares = shares_quantity,
    margins = margins,
    ownerPre = owner_pre,
    insideSize = inside_size,
    labels = as.character(indata$Name)
  )
  if (include_mkt_elast) fit_args$mktElast <- mkt_elast

  ## Linear and log-linear Cournot models are the historical homogeneous-
  ## product route: each input row is a plant and the first observed price is
  ## the single market price.
  if (identical(spec$conduct, "cournot") &&
      spec$demand_id %in% c("linear", "loglin")) {
    first_price <- prices[which(!is.na(prices))[1L]]
    fit_args$prices <- first_price
    fit_args$quantities <- as.matrix(output)
    fit_args$margins <- as.matrix(margins)
    fit_args$shares <- NULL
    fit_args$insideSize <- NULL
    fit_args$labels <- list(as.character(indata$Name), as.character(indata$Name[1L]))
  } else if (spec$demand_id %in% c("linear", "loglin")) {
    fit_args$quantities <- output
  } else if (spec$demand_id %in% c("ces", "aids")) {
    fit_args$shares <- shares_revenue
  } else {
    fit_args$shares <- shares_quantity
  }

  fit <- do.call(antitrust::calibrate, fit_args)
  result <- antitrust::simulate(
    fit,
    ownerPost = owner_post,
    mcDelta = as.numeric(indata$mcDelta)
  )
  list(
    fit = fit, result = result,
    simulation_api = "antitrust::simulate"
  )
}

run_vertical_model <- function(spec, indata, params = list()) {
  owner_pre_down <- as.character(indata$ownerPreDown)
  owner_post_down <- as.character(indata$ownerPostDown)
  owner_pre_up <- as.character(indata$ownerPreUp)
  owner_post_up <- as.character(indata$ownerPostUp)

  fit <- do.call(vertical::calibrate, list(
    demand = spec$upstream_spec,
    prices = as.numeric(indata$pricesDown),
    shares = as.numeric(indata$sharesDown),
    margins = as.numeric(indata$marginsDown),
    ownerPre = owner_pre_down,
    pricesUp = as.numeric(indata$pricesUp),
    marginsUp = as.numeric(indata$marginsUp),
    ownerPreUp = owner_pre_up,
    priceOutside = 0,
    labels = as.character(indata$Name)
  ))

  result <- vertical::simulate(
    fit,
    ownerPost = list(up = owner_post_up, down = owner_post_down)
  )
  list(
    fit = fit, result = result,
    simulation_api = "vertical::simulate"
  )
}
