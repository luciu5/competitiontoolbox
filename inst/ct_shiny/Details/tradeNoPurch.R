tradeNoPurch <- function(sim) {

  if(!model_supports_no_purchase_share(sim)){return()}

  isCES <- model_uses_revenue_shares(sim)

  res <- data.frame('No-purchase\n Share (%)'= c(
    1 - sum(trade::calcShares(sim, preMerger=TRUE, revenue=isCES)),
    1 - sum(trade::calcShares(sim, preMerger=FALSE, revenue=isCES))), check.names = FALSE)*100

  res$'Revenues ($)' <- as.integer(round(c(trade::calcRevenues(sim, preMerger=TRUE, market = TRUE),
                                           trade::calcRevenues(sim, preMerger=FALSE, market = TRUE))))

  spec <- model_result_spec(sim)
  policy_label <- if(identical(spec$page, "Quotas")) "Quota" else "Tariff"
  rownames(res) <- paste(c("Current", "New"), policy_label)

  if(identical(spec$demand_id, "aids")) res$'No-purchase\n Share (%)' <- NULL

  return(res)
}
