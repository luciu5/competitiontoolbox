
tradeNoPurch <- function(sim) {

  if(!model_supports_no_purchase_share(sim)){return()}

  isCES <- model_uses_revenue_shares(sim)

  res <- data.frame('No-purchase\n Share (%)'= c(
    1 - sum(calcShares(sim, preMerger=TRUE,revenue=isCES)),
    1 - sum(calcShares(sim, preMerger=FALSE,revenue=isCES))), check.names = FALSE)*100

  res$'Revenues ($)' <- as.integer(round(c(calcRevenues(sim, preMerger=TRUE, market = TRUE),
                                           calcRevenues(sim, preMerger=FALSE, market = TRUE))))

  rownames(res) <- c("Current Tariff", "New Tariff")

  spec <- attr(sim, "ct_model_spec")
  if(!is.null(spec) && identical(spec$demand_id, "aids")) res$'No-purchase\n Share (%)' <- NULL
  if(is.null(spec) && grepl("aids", class(sim), ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

  return(res)
}
