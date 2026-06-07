model_registry <- function() {
  known <- "known"
  unknown <- "unknown"
  none <- "none"

  data.frame(
    page = c(
      rep("Horizontal", 14),
      rep("Vertical", 2),
      rep("Tariffs", 16),
      rep("Quotas", 2)
    ),
    supply = c(
      rep("Bertrand", 6),
      rep("Cournot", 6),
      rep("2nd Score Auction", 2),
      "Bertrand", "2nd Score Auction",
      rep("Bertrand", 6),
      rep("Cournot", 6),
      rep("Monopolistic Competition", 4),
      rep("Bertrand", 2)
    ),
    demand_label = c(
      "logit", "ces", "aids",
      "logit (unknown elasticity)", "ces (unknown elasticity)", "aids (unknown elasticity)",
      "logit", "linear", "loglinear",
      "logit (unknown elasticity)", "linear (unknown elasticity)", "loglinear (unknown elasticity)",
      "logit", "logit (unknown elasticity)",
      "logit", "logit",
      "logit", "ces", "aids",
      "logit (unknown elasticity)", "ces (unknown elasticity)", "aids (unknown elasticity)",
      "logit", "linear", "loglinear",
      "logit (unknown elasticity)", "linear (unknown elasticity)", "loglinear (unknown elasticity)",
      "logit", "ces", "logit (unknown elasticity)", "ces (unknown elasticity)",
      "logit", "logit (unknown elasticity)"
    ),
    demand_id = c(
      "logit", "ces", "aids", "logit", "ces", "aids",
      "logit", "linear", "loglinear", "logit", "linear", "loglinear",
      "logit", "logit",
      "logit", "logit",
      "logit", "ces", "aids", "logit", "ces", "aids",
      "logit", "linear", "loglinear", "logit", "linear", "loglinear",
      "logit", "ces", "logit", "ces",
      "logit", "logit"
    ),
    calibration = c(
      rep(known, 3), rep(unknown, 3),
      rep(known, 3), rep(unknown, 3),
      known, unknown,
      none, none,
      rep(known, 3), rep(unknown, 3),
      rep(known, 3), rep(unknown, 3),
      rep(known, 2), rep(unknown, 2),
      known, unknown
    ),
    simulation_fn = c(
      "logit.alm", "ces.alm", "aids", "logit.alm", "ces.alm", "aids",
      "logit.cournot.alm", "cournot", "cournot", "logit.cournot.alm", "cournot", "cournot",
      "auction2nd.logit.alm", "auction2nd.logit.alm",
      "vertical.barg", "vertical.barg",
      "bertrand_tariff", "bertrand_tariff", "bertrand_tariff",
      "bertrand_tariff", "bertrand_tariff", "bertrand_tariff",
      "logit_cournot_tariff", "cournot_tariff", "cournot_tariff",
      "logit_cournot_tariff", "cournot_tariff", "cournot_tariff",
      "monopolistic_competition_tariff", "monopolistic_competition_tariff",
      "monopolistic_competition_tariff", "monopolistic_competition_tariff",
      "bertrand_quota", "bertrand_quota"
    ),
    output_shape = c(
      rep("shares", 6),
      "full_elasticity_matrix", "quantity_table", "quantity_table",
      "full_elasticity_matrix", "quantity_table", "quantity_table",
      rep("shares", 2),
      rep("shares", 2),
      rep("shares", 6),
      "full_elasticity_matrix", "quantity_table", "quantity_table",
      "full_elasticity_matrix", "quantity_table", "quantity_table",
      rep("shares", 4),
      rep("shares", 2)
    ),
    stringsAsFactors = FALSE
  )
}

model_demand_id <- function(demand_label) {
  gsub("\\s*\\(.*", "", demand_label, perl = TRUE)
}

model_has_known_elasticity <- function(calc_elast) {
  grepl("elasticity", calc_elast, ignore.case = TRUE)
}

model_spec <- function(page, supply, demand) {
  registry <- model_registry()
  demand_id <- model_demand_id(demand)
  matches <- registry[
    registry$page == page &
      registry$supply == supply &
      registry$demand_id == demand_id &
      registry$calibration == ifelse(grepl("unknown elasticity", demand, ignore.case = TRUE), "unknown", registry$calibration),
  ]

  if (nrow(matches) != 1) {
    matches <- registry[
      registry$page == page &
        registry$supply == supply &
        registry$demand_label == demand,
    ]
  }

  if (nrow(matches) != 1) {
    stop(
      sprintf("Unsupported model combination: page=%s, supply=%s, demand=%s", page, supply, demand),
      call. = FALSE
    )
  }

  as.list(matches[1, ])
}

model_demand_choices <- function(page, supply, known_elasticity = TRUE) {
  registry <- model_registry()
  calibration <- if (isTRUE(known_elasticity)) "known" else "unknown"
  choices <- registry$demand_label[
    registry$page == page &
      registry$supply == supply &
      registry$calibration %in% c(calibration, "none")
  ]
  unique(choices)
}

model_is_cournot <- function(sim) {
  spec <- attr(sim, "ct_model_spec")
  if (!is.null(spec)) {
    return(identical(spec$supply, "Cournot"))
  }
  grepl("Cournot", class(sim), ignore.case = TRUE)
}

model_is_auction <- function(sim) {
  spec <- attr(sim, "ct_model_spec")
  if (!is.null(spec)) {
    return(identical(spec$supply, "2nd Score Auction"))
  }
  grepl("Auction", class(sim), ignore.case = TRUE)
}

model_is_vertical <- function(sim) {
  spec <- attr(sim, "ct_model_spec")
  if (!is.null(spec)) {
    return(identical(spec$page, "Vertical"))
  }
  grepl("Vert", class(sim), ignore.case = TRUE)
}

model_uses_revenue_shares <- function(sim) {
  spec <- attr(sim, "ct_model_spec")
  if (!is.null(spec)) {
    return(spec$demand_id %in% c("ces", "aids"))
  }
  grepl("ces|aids", class(sim), ignore.case = TRUE)
}

model_supports_no_purchase_share <- function(sim) {
  !model_is_cournot(sim)
}

model_supports_diversion <- function(sim) {
  !model_is_cournot(sim)
}

model_has_quantities <- function(sim) {
  "quantities" %in% slotNames(sim)
}

format_elasticity_table <- function(res, sim) {
  res <- as.matrix(res)
  if (model_is_cournot(sim) && ncol(res) == 1) {
    colnames(res) <- "Elasticity"
  }
  res
}

normalize_model_inputs <- function(page, indata) {
  if (is.null(indata)) {
    return(indata)
  }

  if (page != "Vertical") {
    cnames <- colnames(indata)
    cnames[grepl("^Prices", cnames, ignore.case = TRUE)] <- "Prices"
    cnames[grepl("^Margins", cnames, ignore.case = TRUE)] <- "Margins"
    cnames[grepl("Quantities|Revenues", cnames, ignore.case = TRUE)] <- "Output"
    cnames[grepl("Cost Changes", cnames, ignore.case = TRUE)] <- "mcDeltaInput"
    cnames[grepl("^Current.*(Tariff|Quota)", cnames, ignore.case = TRUE)] <- "tariffPre"
    cnames[grepl("^New.*(Tariff|Quota)", cnames, ignore.case = TRUE)] <- "tariffPost"
    colnames(indata) <- cnames
  }

  if (page %in% c("Horizontal", "Tariffs", "Quotas")) {
    indata <- indata[!is.na(indata$Output), , drop = FALSE]
    if (!"mcDelta" %in% colnames(indata)) {
      indata$mcDelta <- 0
    }
  }

  if (page == "Horizontal") {
    if ("mcDeltaInput" %in% colnames(indata)) {
      indata$mcDelta <- indata$mcDeltaInput
      indata$mcDelta[is.na(indata$mcDelta)] <- 0
    }
    indata$`Pre-merger\n Owner` <- factor(indata$`Pre-merger\n Owner`, levels = unique(indata$`Pre-merger\n Owner`))
    indata$`Post-merger\n Owner` <- factor(indata$`Post-merger\n Owner`, levels = unique(indata$`Post-merger\n Owner`))
  }

  if (page == "Vertical") {
    indata <- indata[!is.na(indata$sharesDown), , drop = FALSE]
  }

  if (page %in% c("Tariffs", "Quotas")) {
    indata$Owner <- factor(indata$Owner, levels = unique(indata$Owner))
  }

  if (page == "Tariffs") {
    indata$tariffPre[is.na(indata$tariffPre)] <- 0
    indata$tariffPost[is.na(indata$tariffPost)] <- 0
    indata$mcDelta <- (indata$tariffPost - indata$tariffPre) / (1 - indata$tariffPost)
  }

  if (page == "Quotas") {
    indata$tariffPre[is.na(indata$tariffPre)] <- Inf
    indata$tariffPost[is.na(indata$tariffPost)] <- Inf
  }

  indata
}

run_model <- function(spec, data, params = list()) {
  page <- spec$page
  data <- normalize_model_inputs(page, data)
  fn <- get(spec$simulation_fn)
  mktElast <- params$mktElast
  known_elast <- identical(spec$calibration, "known")

  result <- switch(
    page,
    Horizontal = run_horizontal_model(fn, spec, data, mktElast, known_elast),
    Vertical = run_vertical_model(fn, spec, data),
    Tariffs = run_tariff_model(fn, spec, data, mktElast, known_elast),
    Quotas = run_quota_model(fn, spec, data, mktElast, known_elast),
    stop(sprintf("Unsupported page: %s", page), call. = FALSE)
  )

  attr(result, "ct_model_spec") <- spec
  result
}

run_horizontal_model <- function(fn, spec, indata, mktElast, known_elast) {
  prices <- indata$Prices
  margins <- indata$Margins
  missPrices <- any(is.na(prices))
  shares_quantity <- shares_revenue <- indata$Output / sum(indata$Output, na.rm = TRUE)
  insideSize <- sum(indata$Output, na.rm = TRUE)

  if (!missPrices) {
    if (spec$demand_id %in% c("ces", "aids")) {
      insideSize <- sum(prices * indata$Output, na.rm = TRUE)
    }
    shares_revenue <- prices * shares_revenue / sum(prices * shares_revenue)
    if (identical(spec$supply, "2nd Score Auction")) {
      margins <- margins * prices
    }
  }

  ownerPre <- tcrossprod(model.matrix(~ -1 + indata$`Pre-merger\n Owner`))
  if (nlevels(indata$`Post-merger\n Owner`) > 1) {
    ownerPost <- tcrossprod(model.matrix(~ -1 + indata$`Post-merger\n Owner`))
  } else {
    ownerPost <- matrix(1, ncol = length(indata$Output), nrow = length(indata$Output))
  }

  if (identical(spec$supply, "Cournot") && spec$demand_id != "logit") {
    args <- list(
      prices = na.omit(prices)[1],
      demand = ifelse(spec$demand_id == "loglinear", "log", "linear"),
      quantities = as.matrix(indata$Output),
      margins = as.matrix(margins),
      ownerPre = ownerPre,
      ownerPost = ownerPost,
      mktElast = ifelse(known_elast, mktElast, NA_real_),
      mcDelta = indata$mcDelta,
      labels = list(indata$Name, indata$Name[1])
    )
    return(do.call(fn, args))
  }

  shares <- if (spec$demand_id %in% c("ces", "aids")) shares_revenue else shares_quantity
  args <- list(
    prices = prices,
    shares = shares,
    margins = margins,
    ownerPre = ownerPre,
    ownerPost = ownerPost,
    insideSize = insideSize,
    mcDelta = indata$mcDelta,
    labels = indata$Name
  )
  if (known_elast || identical(spec$supply, "Cournot")) {
    args$mktElast <- ifelse(known_elast, mktElast, NA_real_)
  }

  do.call(fn, args)
}

run_vertical_model <- function(fn, spec, indata) {
  supplyDown <- ifelse(identical(spec$supply, "Bertrand"), "bertrand", "2nd")
  do.call(fn, list(
    supplyDown = supplyDown,
    sharesDown = indata$sharesDown,
    pricesDown = indata$pricesDown,
    marginsDown = indata$marginsDown,
    ownerPreDown = indata$ownerPreDown,
    ownerPostDown = indata$ownerPostDown,
    pricesUp = indata$pricesUp,
    marginsUp = indata$marginsUp,
    ownerPreUp = indata$ownerPreUp,
    ownerPostUp = indata$ownerPostUp,
    priceOutside = 0,
    labels = indata$Name
  ))
}

run_tariff_model <- function(fn, spec, indata, mktElast, known_elast) {
  if (identical(spec$supply, "Cournot") && spec$demand_id != "logit") {
    args <- list(
      prices = na.omit(indata$Prices)[1],
      demand = ifelse(spec$demand_id == "loglinear", "log", "linear"),
      quantities = as.matrix(indata$Output),
      margins = as.matrix(indata$Margins),
      owner = indata$Owner,
      tariffPre = as.matrix(indata$tariffPre),
      tariffPost = as.matrix(indata$tariffPost),
      labels = list(indata$Name, indata$Name[1])
    )
    if (known_elast) {
      args$mktElast <- mktElast
    }
    return(do.call(fn, args))
  }

  args <- list(
    prices = indata$Prices,
    quantities = indata$Output,
    margins = indata$Margins,
    tariffPre = indata$tariffPre,
    tariffPost = indata$tariffPost,
    labels = indata$Name
  )

  if (!identical(spec$supply, "Monopolistic Competition")) {
    args$owner <- indata$Owner
  }
  if (!identical(spec$supply, "Cournot") || spec$demand_id != "logit") {
    args$demand <- spec$demand_id
  }
  if (known_elast || identical(spec$supply, "Cournot")) {
    args$mktElast <- ifelse(known_elast, mktElast, NA_real_)
  }

  do.call(fn, args)
}

run_quota_model <- function(fn, spec, indata, mktElast, known_elast) {
  args <- list(
    demand = spec$demand_id,
    prices = indata$Prices,
    quantities = indata$Output,
    margins = indata$Margins,
    owner = indata$Owner,
    quotaPre = indata$tariffPre,
    quotaPost = indata$tariffPost,
    labels = indata$Name
  )
  if (known_elast) {
    args$mktElast <- mktElast
  }
  do.call(fn, args)
}
