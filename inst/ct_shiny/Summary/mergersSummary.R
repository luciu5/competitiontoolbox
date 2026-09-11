
mergersSummary <- function(res){
  #a function to generate summary stats for results tab

  isCournot <- model_is_cournot(res)
  isAuction <- model_is_auction(res)
  isRevDemand <- model_uses_revenue_shares(res)

  inputs <- model_result_inputs(res)
  spec <- model_result_spec(res)
  missPrices <- anyNA(inputs$Prices)

  inLevels <- FALSE

  if(isAuction && missPrices){inLevels = TRUE}

  if(isCournot){

    capture.output(s <- antitrust::summary(res, market = FALSE))

    ## Homogeneous-product Cournot summaries report plant quantities, whereas
    ## differentiated Logit-Cournot summaries report product shares.  Retain
    ## the historical share-weighting used below from the public summary.
    if ("quantityPost" %in% names(s)) {
      totQuantPost <- sum(s$quantityPost, na.rm = TRUE)
      s$sharesPost <- s$quantityPost / totQuantPost * 100
    }

  }

  else{
    capture.output(s <- antitrust::summary(res, revenue = isRevDemand & missPrices,levels = inLevels))

  }

  isparty <- s$isParty == "*"

  thispsdelta <- thiscmcr <- thiscv <- NA

  try(thiscmcr <- antitrust::cmcr(res), silent=TRUE)
  try(thiscv <- antitrust::CV(res),silent = TRUE)

  try(thispsdelta  <- sum(antitrust::calcProducerSurplus(res,preMerger=FALSE) - antitrust::calcProducerSurplus(res,preMerger=TRUE)),silent=TRUE)

  partyshare <- s$sharesPost[isparty]

  ## Concentration screens use the same pre-merger shares for both ownership
  ## states.  This preserves the legacy summary's merger-index convention
  ## while obtaining shares and ownership through public data/API surfaces.
  theseshares <- if (isCournot &&
                     spec$demand_id %in% c("linear", "loglin")) {
    inputs$Output / sum(inputs$Output, na.rm = TRUE)
  } else {
    antitrust::calcShares(
      res, preMerger = TRUE, revenue = isRevDemand & missPrices
    )
  }
  theseshares <- theseshares / sum(theseshares, na.rm = TRUE)
  owner_pre <- inputs[["Pre-merger\n Owner"]]
  owner_post <- inputs[["Post-merger\n Owner"]]

  result <- with(s, data.frame(
    'Pre-Merger HHI' = as.integer(antitrust::HHI(
      theseshares, owner = owner_pre
    )),
    'HHI Change' = as.integer(antitrust::HHI(
      theseshares, owner = owner_post
    ) - antitrust::HHI(theseshares, owner = owner_pre)),
    'Industry Price Change (%)' = sum(priceDelta * sharesPost/100,na.rm=TRUE),
    'Merging Party Price Change (%)'= sum(priceDelta[isparty] * partyshare, na.rm=TRUE) / sum(partyshare),
    'Compensating Marginal Cost Reduction (%)' = ifelse(isCournot, thiscmcr, sum(thiscmcr * partyshare) / sum(partyshare)),
    'Consumer Harm ($)' = thiscv,
    'Producer Benefit ($)' = thispsdelta,
    'Difference ($)'= thiscv - thispsdelta,

    #'Estimated Market Elasticity' = thiselast,
    check.names=FALSE
  ))

  ## Append parenthetical with % of post-merger revenues
  result <- rbind(result,result)
  result[2, !grepl("\\$",colnames(result))] <- NA
  result[,-(1:2)] <- round(result[,-(1:2)],digits = 1)
  result[2,grepl("\\$",colnames(result))] <- paste0("(",round(result[2 , grepl("\\$",colnames(result))]*100 / antitrust::calcRevenues(res, preMerger = FALSE, market=TRUE), digits = 1), "%)")


  if(inLevels){
    colnames(result) <- gsub('(?<=Price Change\\s)\\(%\\)',"($/unit)",colnames(result), perl=TRUE)
  }

  if(all(is.na(result[,"Compensating Marginal Cost Reduction (%)"]))) result[,"Compensating Marginal Cost Reduction (%)"] <- NULL

  return(result)
}
