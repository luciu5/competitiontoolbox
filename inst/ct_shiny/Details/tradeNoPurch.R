
tradeNoPurch <- function(sim) {

  if(grepl("cournot",class(sim),ignore.case = TRUE)){return()}

  isCES <- grepl("ces",class(sim),ignore.case = TRUE)

  res <- data.frame('No-purchase\n Share (%)'= c(
    1 - sum(calcShares(sim, preMerger=TRUE,revenue=isCES)),
    1 - sum(calcShares(sim, preMerger=FALSE,revenue=isCES))), check.names = FALSE)*100

  res$'Revenues ($)' <- as.integer(round(c(calcRevenues(sim, preMerger=TRUE, market = TRUE),
                                           calcRevenues(sim, preMerger=FALSE, market = TRUE))))

  rownames(res) <- c("Current Tariff", "New Tariff")

  if(grepl("aids",class(sim),ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

  return(res)
}
