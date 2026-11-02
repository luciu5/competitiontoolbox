tradeTemplateCode <- function(type){
  type <- match.arg(type, c("Tariffs", "Quotas"))
  indata <- if (type == "Tariffs") {
    valuesTariffs[["inputData"]]
  } else {
    valuesQuota[["inputData"]]
  }
  indata <- indata[!is.na(indata$Name) & indata$Name != '', , drop = FALSE]

  ## Emit a clean, standalone data object. dput() keeps NA/Inf values and
  ## product/owner labels valid even when labels contain punctuation.
  column <- function(pattern, required = TRUE) {
    hit <- grep(pattern, colnames(indata), ignore.case = TRUE, perl = TRUE)
    if (!length(hit) && required) stop("trade template input is missing ", pattern)
    if (length(hit)) indata[[hit[1L]]] else NULL
  }
  literal <- function(value) {
    value <- if (is.factor(value)) as.character(value) else value
    paste(capture.output(dput(value)), collapse = '')
  }

  values <- list(
    Name = column('^Name$'),
    Owner = column('^Owner$'),
    Prices = column('price'),
    Output = column('Quantities|Revenues'),
    Margins = column('Margin'),
    policyPre = column('^Current'),
    policyPost = column('^New')
  )
  data_code <- paste0(
    'simdata <- data.frame(\n',
    paste(vapply(names(values), function(name) {
      paste0('  ', name, ' = ', literal(values[[name]]))
    }, character(1L)), collapse = ',\n'),
    ',\n  check.names = FALSE, stringsAsFactors = FALSE\n)'
  )

  spec <- model_spec(
    type,
    if (type == "Tariffs") supply() else supply(),
    demand()
  )
  known_elast <- identical(spec$calibration, "known")
  mkt_input <- if (type == "Tariffs") input$enterElastTariffs else input$enterElastQuota
  mkt_code <- if (known_elast) literal(mkt_input) else NULL
  policy <- if (type == "Tariffs") "tariff" else "quota"
  post_arg <- if (type == "Tariffs") "tariffPost" else "quotaPost"
  pre_arg <- if (type == "Tariffs") "tariffPre" else "quotaPre"

  is_cournot_matrix <- identical(spec$conduct, "cournot") &&
    !identical(spec$demand_id, "logit")
  if (is_cournot_matrix) {
    calibration_code <- paste0(
      'first_price <- which(!is.na(simdata$Prices))[1L]\n',
      'calibration_args <- list(\n',
      '  prices = simdata$Prices[first_price],\n',
      '  quantities = as.matrix(simdata$Output),\n',
      '  margins = as.matrix(simdata$Margins),\n',
      '  owner = as.character(simdata$Owner),\n',
      '  ', pre_arg, ' = as.matrix(simdata$policyPre),\n',
      '  labels = list(as.character(simdata$Name), as.character(simdata$Name[first_price]))\n',
      ')'
    )
    post_code <- paste0('as.matrix(simdata$policyPost)')
  } else {
    calibration_code <- paste0(
      'calibration_args <- list(\n',
      '  prices = simdata$Prices,\n',
      '  quantities = simdata$Output,\n',
      '  margins = simdata$Margins,\n',
      '  ', pre_arg, ' = simdata$policyPre,\n',
      '  labels = as.character(simdata$Name)\n',
      ')'
    )
    if (!identical(spec$conduct, "moncom")) {
      calibration_code <- paste0(
        calibration_code,
        '\ncalibration_args$owner <- as.character(simdata$Owner)'
      )
    }
    post_code <- 'simdata$policyPost'
  }
  if (known_elast) {
    calibration_code <- paste0(
      calibration_code,
      '\ncalibration_args$mktElast <- ', mkt_code
    )
  }

  simulation_code <- paste0(
    'simres <- trade::simulate(fit, ', post_arg, ' = ', post_code, ')\n'
  )

  model_code <- paste0(
    'trade_spec <- trade::model_spec(\n',
    '  demand = ', shQuote(spec$demand_id), ',\n',
    '  conduct = ', shQuote(spec$conduct), ',\n',
    '  variant = ', shQuote(spec$variant), ',\n',
    '  policy = ', shQuote(policy), '\n',
    ')\n',
    calibration_code, '\n',
    'fit <- do.call(trade::calibrate, c(list(demand = trade_spec), calibration_args))\n',
    simulation_code
  )

  revenue_code <- if (spec$demand_id %in% c("ces", "aids")) {
    "anyNA(simdata$Prices)"
  } else {
    "FALSE"
  }
  market_summary_code <- if (identical(spec$demand_id, "aids")) {
    c(
      "if (anyNA(simdata$Prices)) {",
      "  details <- NULL",
      "  capture.output(details <- trade::summary(simres, revenue = TRUE, levels = FALSE, market = FALSE))",
      "  is_foreign <- trimws(as.character(details$isForeign)) == \"*\"",
      "  weighted_change <- function(select) {",
      "    if (!any(select)) return(NA_real_)",
      "    stats::weighted.mean(details$priceDelta[select], details$sharesPost[select], na.rm = TRUE)",
      "  }",
      "  market_summary <- data.frame(",
      "    `Domestic Firm Price Change (%)` = weighted_change(!is_foreign),",
      "    `Foreign Firm Price Change (%)` = weighted_change(is_foreign),",
      "    `Industry Price Change (%)` = weighted_change(rep(TRUE, nrow(details))),",
      "    check.names = FALSE",
      "  )",
      "  print(market_summary, row.names = FALSE)",
      "} else {",
      "  trade::summary(simres, levels = FALSE, market = TRUE)",
      "}"
    )
  } else {
    "trade::summary(simres, levels = FALSE, market = TRUE)"
  }

  c(
    'library(trade)',
    '',
    '## Load Data:',
    data_code,
    '',
    '## Calibrate baseline and run one counterfactual:',
    model_code,
    '## Summary Tab Results:',
    market_summary_code,
    '',
    '## Details Tab Results:',
    paste0('trade::summary(simres, revenue = ', revenue_code,
           ', levels = FALSE, market = FALSE)'),
    '',
    '## Elasticities Tab Results (Pre-policy Only):',
    'trade::elast(simres, preMerger = TRUE, market = TRUE)',
    'trade::elast(simres, preMerger = TRUE, market = FALSE)',
    '',
    '## Diagnostics Tab Results:',
    'trade::calcDiagnostics(simres)',
    ''
  )
}
