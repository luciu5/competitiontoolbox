
mergersSummary <- function(res){
  #a function to generate summary stats for results tab

  isCournot <- grepl("Cournot",class(res))
  isAuction <- grepl("Auction",class(res))
  isRevDemand <- grepl("ces|aids",class(res),ignore.case = TRUE)
  isLogit <- grepl("logit",class(res),ignore.case = TRUE)

  missPrices <- any(is.na(res@prices))

  inLevels <- FALSE

  if(isAuction && missPrices){inLevels = TRUE}

  if(isCournot){

    capture.output(s <- summary(res, market = FALSE))
    theseshares <- drop(res@quantities/sum(res@quantities))
    # Fixed HHI share calculations for Cournot results
    totQuantPost <- sum(s$quantityPost,na.rm=TRUE)
    s$sharesPost <- s$quantityPost/totQuantPost*100

  }

  else{
    capture.output(s <- summary(res, revenue = isRevDemand & missPrices,levels = inLevels))
    theseshares <- calcShares(res, preMerger=TRUE, revenue=isRevDemand & missPrices)

    theseshares <- theseshares/sum(theseshares)

  }

  isparty <- s$isParty == "*"

  thispsdelta <- thiscmcr <- thiscv <- NA

  try(thiscmcr <- cmcr(res), silent=TRUE)
  try(thiscv <- CV(res),silent = TRUE)

  try(thispsdelta  <- sum(calcProducerSurplus(res,preMerger=FALSE) - calcProducerSurplus(res,preMerger=TRUE)),silent=TRUE)

  partyshare <- s$sharesPost[isparty]

  result <- with(s, data.frame(
    #'HHI Change' = as.integer(round(hhi(res,preMerger=FALSE) -  hhi(res,preMerger=TRUE))),
    'Pre-Merger HHI' =  as.integer(HHI(theseshares,owner=res@ownerPre)),
    'HHI Change' = as.integer(HHI(theseshares,owner=res@ownerPost) - HHI(theseshares,owner=res@ownerPre)),
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
  result[2,grepl("\\$",colnames(result))] <- paste0("(",round(result[2 , grepl("\\$",colnames(result))]*100 / calcRevenues(res, preMerger = FALSE, market=TRUE), digits = 1), "%)")


  if(inLevels){
    colnames(result) <- gsub('(?<=Price Change\\s)\\(%\\)',"($/unit)",colnames(result), perl=TRUE)
  }

  if(all(is.na(result[,"Compensating Marginal Cost Reduction (%)"]))) result[,"Compensating Marginal Cost Reduction (%)"] <- NULL

  return(result)
}
