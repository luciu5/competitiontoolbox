tradeDiag <- function(res, mktElast = FALSE){
  ## Observed values come from canonical input metadata. Fitted values are
  ## recovered through trade's public calculation methods; no trade S4 slots
  ## (or private package helpers) are needed by the diagnostics tab.
  spec <- model_result_spec(res)
  indata <- model_result_inputs(res)
  isCournot <- model_is_cournot(res)
  isCournotMatrix <- isCournot && !identical(spec$demand_id, "logit")
  isRevDemand <- model_uses_revenue_shares(res)

  labels <- as.character(indata$Name)
  if (isCournotMatrix) {
    firstPrice <- which(!is.na(indata$Prices))[1]
    obsPrices <- indata$Prices[firstPrice]
    obsShares <- as.matrix(indata$Output / sum(indata$Output, na.rm = TRUE))
    obsMargins <- as.matrix(indata$Margins)
  } else {
    obsPrices <- indata$Prices
    quantityShares <- indata$Output / sum(indata$Output, na.rm = TRUE)
    if (isRevDemand && all(!is.na(indata$Prices))) {
      obsShares <- indata$Prices * quantityShares /
        sum(indata$Prices * quantityShares, na.rm = TRUE)
    } else {
      obsShares <- quantityShares
    }
    obsMargins <- indata$Margins
  }

  ## The route retains the entered market elasticity as metadata because the
  ## legacy S4 field is intentionally not part of the app-facing contract.
  obsElast <- attr(res, "ct_mktElast", exact = TRUE)
  if (is.null(obsElast)) obsElast <- NA_real_

  prePrices <- unname(drop(trade::calcPrices(res, preMerger = TRUE)))
  preMargins <- drop(trade::calcMargins(res, preMerger = TRUE))
  preShares <- drop(trade::calcShares(
    res, preMerger = TRUE, revenue = isRevDemand
  ))
  preShares <- drop(preShares / sum(preShares))
  preElast <- trade::elast(res, preMerger = TRUE, market = TRUE)

  if(!mktElast){
    result <- data.frame(
      "Inputted Prices" = obsPrices,
      "Fitted Prices" = prePrices,
      "Price Change (%)" = (1 - obsPrices / prePrices) * 100,
      "Inputted Shares (%)" = obsShares * 100,
      "Fitted Shares(%)" = preShares * 100,
      "Share Change (%)" = (1 - obsShares / preShares) * 100,
      "Inputted Margins" = obsMargins,
      "Fitted  Margins" = preMargins,
      "Margin Change (%)" = (1 - obsMargins / preMargins) * 100,
      check.names = FALSE
    )

    if(isCournot) result[-1,grepl('Prices',colnames(result))] <- NA
    if(!isCournot) rownames(result) <- labels
  }
  else{
    result <- data.frame(
      'Inputted Elasticity' = obsElast,
      'Fitted Elasticity' = preElast,
      'Elasticity Change'= (1 - obsElast / preElast) * 100,
      check.names = FALSE
    )
  }
  return(result)
}
