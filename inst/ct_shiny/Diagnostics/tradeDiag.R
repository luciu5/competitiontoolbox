
tradeDiag <- function(res, mktElast = FALSE){
  #a function to generate diagnostic data

  isCournot <- grepl("Cournot",class(res))

  if(isCournot){labels= res@labels[[1]]}
  else{labels=res@labels}

  obsPrices <- res@prices
  obsShares <- res@shares
  obsMargins <- res@margins
  obsElast <- res@mktElast

  #if(length(obsMargins[!is.na(obsMargins)]) < 2){return()}

  prePrices <- unname(drop(res@pricePre))
  preMargins <- drop(calcMargins(res, preMerger=TRUE))
  preShares <- drop(calcShares(res, preMerger=TRUE))
  preShares <- drop(preShares/sum(preShares))
  preElast <- elast(res, preMerger=TRUE, market=TRUE)

  if(!mktElast){

    res <- data.frame(
      "Inputted Prices"= obsPrices,
      "Fitted Prices" = prePrices,
      "Price Change (%)"= (1 - obsPrices/prePrices)*100,
      "Inputted Shares (%)" = obsShares*100,
      "Fitted Shares(%)"=preShares*100,
      "Share Change (%)"=(1 - obsShares/preShares)*100,
      "Inputted Margins" = obsMargins,
      "Fitted  Margins"=preMargins,
      "Margin Change (%)"= (1 - obsMargins/preMargins)*100,
      #'Market Elasticity'= 1 - obsElast/preElast,
      check.names = FALSE
    )

    #rmThese <- colSums(abs(res),na.rm=TRUE)
    if(isCournot)  res[-1,grepl('Prices',colnames(res))] <- NA

    #res <- res[,rmThese >1e-3,drop=FALSE]
    if(!isCournot) rownames(res) <- labels
  }

  else{ res <- data.frame(
    'Inputted Elasticity' = obsElast,
    'Fitted Elasticity' = preElast,
    'Elasticity Change'= (1 - obsElast/preElast)*100,
    check.names = FALSE)

  #if(res < 1e-3) res <- NULL
  }
  return(res)
}
