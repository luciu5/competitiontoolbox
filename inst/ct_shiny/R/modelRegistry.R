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
      "logit", "linear", "loglin", "logit", "linear", "loglin",
      "logit", "logit",
      "logit", "logit",
      "logit", "ces", "aids", "logit", "ces", "aids",
      "logit", "linear", "loglin", "logit", "linear", "loglin",
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
    upstream_package = c(
      rep("antitrust", 14),
      rep("vertical", 2),
      rep("trade", 18)
    ),
    conduct = c(
      rep("bertrand", 6),
      rep("cournot", 6),
      rep("auction2nd", 2),
      rep("vertical_bargaining", 2),
      rep("bertrand", 6),
      rep("cournot", 6),
      rep("moncom", 4),
      rep("bertrand", 2)
    ),
    variant = c(
      "alm", "alm", "standard", "alm", "alm", "standard",
      "alm", "standard", "standard", "alm", "standard", "standard",
      "alm", "alm",
      "standard", "auction2nd",
      rep("standard", 6), "alm", rep("standard", 11)
    ),
    policy = c(
      rep(NA_character_, 16),
      rep("tariff", 16),
      rep("quota", 2)
    ),
    stringsAsFactors = FALSE
  )
}

model_demand_id <- function(demand_label) {
  demand_id <- gsub("\\s*\\(.*", "", demand_label, perl = TRUE)
  if (identical(demand_id, "loglinear")) "loglin" else demand_id
}

model_has_known_elasticity <- function(calc_elast) {
  grepl("elasticity", calc_elast, ignore.case = TRUE)
}

upstream_model_spec <- function(spec) {
  switch(
    spec$upstream_package,
    antitrust = antitrust::model_spec(
      demand = spec$demand_id,
      conduct = spec$conduct,
      variant = spec$variant
    ),
    trade = trade::model_spec(
      demand = spec$demand_id,
      conduct = spec$conduct,
      variant = spec$variant,
      policy = spec$policy
    ),
    vertical = vertical::model_spec(
      demand = spec$demand_id,
      conduct = spec$conduct,
      variant = spec$variant
    ),
    stop("Unsupported upstream package: ", spec$upstream_package, call. = FALSE)
  )
}

model_spec <- function(page, supply, demand) {
  registry <- model_registry()
  matches <- registry[
    registry$page == page &
      registry$supply == supply &
      registry$demand_label == demand,
    , drop = FALSE
  ]

  if (nrow(matches) != 1L) {
    stop(
      sprintf("Unsupported model combination: page=%s, supply=%s, demand=%s", page, supply, demand),
      call. = FALSE
    )
  }

  spec <- as.list(matches[1, , drop = FALSE])
  spec$upstream_spec <- upstream_model_spec(spec)
  spec
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

model_result_spec <- function(sim) {
  spec <- attr(sim, "ct_model_spec", exact = TRUE)
  if (is.null(spec)) {
    stop("Result was not created by a competitiontoolbox model route.", call. = FALSE)
  }
  spec
}

model_result_inputs <- function(sim) {
  inputs <- attr(sim, "ct_model_inputs", exact = TRUE)
  if (is.null(inputs)) {
    stop("Result does not retain competitiontoolbox input metadata.", call. = FALSE)
  }
  inputs
}

model_is_cournot <- function(sim) {
  identical(model_result_spec(sim)$conduct, "cournot")
}

model_is_auction <- function(sim) {
  spec <- model_result_spec(sim)
  identical(spec$conduct, "auction2nd") || identical(spec$variant, "auction2nd")
}

model_is_vertical <- function(sim) {
  identical(model_result_spec(sim)$page, "Vertical")
}

model_uses_revenue_shares <- function(sim) {
  model_result_spec(sim)$demand_id %in% c("ces", "aids")
}

model_supports_no_purchase_share <- function(sim) {
  !model_is_cournot(sim)
}

model_supports_diversion <- function(sim) {
  !model_is_cournot(sim)
}

format_elasticity_table <- function(res, sim) {
  res <- as.matrix(res)
  if (model_is_cournot(sim) && ncol(res) == 1L) {
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
    cnames[grepl("^Current.*(Tariff|Quota)", cnames, ignore.case = TRUE)] <- "policyPre"
    cnames[grepl("^New.*(Tariff|Quota)", cnames, ignore.case = TRUE)] <- "policyPost"
    colnames(indata) <- cnames
  }

  if (page %in% c("Horizontal", "Tariffs", "Quotas")) {
    indata <- indata[!is.na(indata$Output), , drop = FALSE]
  }

  if (page == "Horizontal") {
    indata$mcDelta <- 0
    if ("mcDeltaInput" %in% colnames(indata)) {
      indata$mcDelta <- indata$mcDeltaInput
      indata$mcDelta[is.na(indata$mcDelta)] <- 0
    }
    indata$`Pre-merger\n Owner` <- factor(
      indata$`Pre-merger\n Owner`,
      levels = unique(indata$`Pre-merger\n Owner`)
    )
    indata$`Post-merger\n Owner` <- factor(
      indata$`Post-merger\n Owner`,
      levels = unique(indata$`Post-merger\n Owner`)
    )
  }

  if (page == "Vertical") {
    indata <- indata[!is.na(indata$sharesDown), , drop = FALSE]
  }

  if (page %in% c("Tariffs", "Quotas")) {
    indata$Owner <- factor(indata$Owner, levels = unique(indata$Owner))
  }

  if (page == "Tariffs") {
    indata$policyPre[is.na(indata$policyPre)] <- 0
    indata$policyPost[is.na(indata$policyPost)] <- 0
  }

  if (page == "Quotas") {
    indata$policyPre[is.na(indata$policyPre)] <- Inf
    indata$policyPost[is.na(indata$policyPost)] <- Inf
  }

  indata
}

run_model <- function(spec, data, params = list()) {
  data <- normalize_model_inputs(spec$page, data)
  execution <- switch(
    spec$page,
    Horizontal = run_horizontal_model(spec, data, params),
    Vertical = run_vertical_model(spec, data, params),
    Tariffs = run_tariff_model(spec, data, params),
    Quotas = run_quota_model(spec, data, params),
    stop(sprintf("Unsupported page: %s", spec$page), call. = FALSE)
  )

  if (!is.list(execution) || is.null(execution$fit) || is.null(execution$result) ||
      length(execution$simulation_api) != 1L ||
      !nzchar(execution$simulation_api)) {
    stop(
      "A model route must return a fitted baseline, simulated result, and public execution API.",
      call. = FALSE
    )
  }

  result <- execution$result
  attr(result, "ct_model_spec") <- spec
  attr(result, "ct_model_inputs") <- data
  attr(result, "ct_fit") <- execution$fit
  attr(result, "ct_simulation_api") <- execution$simulation_api
  result
}
