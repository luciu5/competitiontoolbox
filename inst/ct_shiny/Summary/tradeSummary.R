
tradeSummary <- function(res, indata,type = c("Tariffs", "Quotas")){
  #a function to generate summary stats for results tab

  type=match.arg(type)

  isCournot <- grepl("Cournot",class(res))
  isAuction <- grepl("Auction",class(res))
  isRevDemand <- grepl("ces|aids",class(res),ignore.case = TRUE)
  isLogit <- grepl("logit",class(res),ignore.case = TRUE)

  missPrices <- any(is.na(res@prices))

  inLevels <- FALSE

  if(isAuction && missPrices){inLevels = TRUE}

  indata <- indata[!is.na(indata$Name),]

  tariffPre <- indata[,grepl("Cur.*\\n(Tariff|Quota)",colnames(indata), perl= TRUE),drop=TRUE]
  tariffPost <- indata[,grepl("New.*\\n(Tariff|Quota)",colnames(indata), perl=TRUE),drop=TRUE]

  if(type == "Tariffs"){
    tariffPre[is.na(tariffPre)] <- 0
    tariffPost[is.na(tariffPost)] <- 0
    istaxed <- tariffPre > 0 | tariffPost > 0
  }
  else if (type=="Quotas"){
    ## set quota for unconstrained firms to be Infinite
    istaxed <- is.finite(tariffPre) | is.finite(tariffPost)
  }


  if(isCournot){
    capture.output( s <- summary(res, market = FALSE))
    # theseshares <- drop(res@quantities/sum(res@quantities))
    #
    # totQuantPost <- sum(s$quantityPost,na.rm=TRUE)
    # s$sharesPost <- s$quantityPost/totQuantPost*100
  }

  else{
    capture.output(s <- summary(res, revenue = isRevDemand & missPrices,levels = inLevels))
    theseshares <- calcShares(res, preMerger=TRUE, revenue=isRevDemand & missPrices)

    theseshares <- theseshares/sum(theseshares)
  }

  thisgovrev <- thispsdelta <- thiscv <- NA

  try(thiscv <- CV(res),silent = TRUE)

  if(type == "Quotas"){
    tariffPre[] <- 0
    tariffPost[] <- 0
  }

  try(thisgovrev <- sum(tariffPost * calcRevenues(res, preMerger = FALSE) - tariffPre * calcRevenues(res, preMerger = TRUE), na.rm=TRUE))
  try(thispsdelta  <- tapply(drop(calcProducerSurplus(res,preMerger=FALSE)*(1 - tariffPost)) - drop(calcProducerSurplus(res,preMerger=TRUE)*(1 - tariffPre)), istaxed,sum),silent=TRUE)

  foreignshare <- s$sharesPost[istaxed]
  domesticshare <- s$sharesPost[!istaxed]

  res <- with(s, data.frame(
    #'Current Tariff HHI' = as.integer(round(hhi(res,preMerger=TRUE))),
    #'HHI Change' = as.integer(round(hhi(res,preMerger=FALSE) -  hhi(res,preMerger=TRUE))),
    'Domestic Firm Price Change (%)'= sum(priceDelta[!istaxed] * domesticshare, na.rm=TRUE) / sum(domesticshare),
    'Foreign Firm Price Change (%)'= sum(priceDelta[istaxed] * foreignshare, na.rm=TRUE) / sum(foreignshare),
    'Industry Price Change (%)' = sum(priceDelta * sharesPost/100,na.rm=TRUE),
    'Consumer Harm ($)' = thiscv,
    'Domestic Firm Benefit ($)' = thispsdelta[1],
    'Foreign Firm Harm ($)' = -thispsdelta[2],
    `Gov't Revenue ($)` = thisgovrev,
    'Net Domestic Harm ($)'= thiscv - thispsdelta[1]  - thisgovrev,
    'Net Total Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2] - thisgovrev,

    #'Estimated Market Elasticity' = thiselast,
    check.names=FALSE
  ))

  if(inLevels){
    colnames(res) <- gsub('(?<=Price Change\\s)\\(%\\)',"($/unit)",colnames(res), perl=TRUE)
  }

  return(res)
}
