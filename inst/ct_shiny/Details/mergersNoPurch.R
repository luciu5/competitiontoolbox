
mergersNoPurch <- function(sim) {

  if(!model_supports_no_purchase_share(sim)){return()}

  isCES <- model_uses_revenue_shares(sim)
  calc_shares <- if (model_is_vertical(sim)) vertical::calcShares else antitrust::calcShares
  calc_revenues <- if (model_is_vertical(sim)) vertical::calcRevenues else antitrust::calcRevenues

  res <- data.frame('No-purchase\n Share (%)'= c(
            1 - sum(calc_shares(sim, preMerger = TRUE, revenue = isCES)),
            1 - sum(calc_shares(sim, preMerger = FALSE, revenue = isCES))), check.names = FALSE)*100

  res$'Revenues ($)' <- as.integer(round(c(calc_revenues(sim, preMerger = TRUE, market = TRUE),
                                           calc_revenues(sim, preMerger = FALSE, market = TRUE))))

  rownames(res) <- c("Pre-Merger", "Post-Merger")

  spec <- model_result_spec(sim)
  if(identical(spec$demand_id, "aids")) res$'No-purchase\n Share (%)' <- NULL

  return(res)
}
