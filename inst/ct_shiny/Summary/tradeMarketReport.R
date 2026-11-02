tradeMarketReport <- function(sim) {
  spec <- model_result_spec(sim)
  inputs <- model_result_inputs(sim)
  missing_prices_aids <- identical(spec$demand_id, "aids") &&
    anyNA(inputs$Prices)

  if (!missing_prices_aids) {
    result <- NULL
    capture.output(result <- trade::summary(
      sim, levels = FALSE, market = TRUE
    ))
    return(as.data.frame(result))
  }

  ## trade's public market summary currently cannot aggregate a price-free
  ## TariffAIDS result.  Its public product summary does expose the revenue
  ## shares and price changes needed for the non-monetary market report.
  ## Monetary welfare is intentionally omitted because prices were not
  ## supplied and therefore market-level dollar values are unidentified.
  details <- NULL
  capture.output(details <- trade::summary(
    sim, revenue = TRUE, levels = FALSE, market = FALSE
  ))
  is_foreign <- trimws(as.character(details$isForeign)) == "*"
  weighted_change <- function(select) {
    if (!any(select)) return(NA_real_)
    stats::weighted.mean(
      details$priceDelta[select], details$sharesPost[select], na.rm = TRUE
    )
  }

  data.frame(
    'Domestic Firm Price Change (%)' = weighted_change(!is_foreign),
    'Foreign Firm Price Change (%)' = weighted_change(is_foreign),
    'Industry Price Change (%)' = weighted_change(rep(TRUE, nrow(details))),
    check.names = FALSE
  )
}
