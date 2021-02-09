
mergersDiag <- function(res, mktElast = FALSE){
  #a function to generate diagnostic data

  # isCournot <- grepl("Cournot", class(res))
  # isVertical <- grepl("Vert", class(res))
  #
  # if(isCournot){labels = res@labels[[1]]}
  # else if(isVertical){labels = res@down@labels}
  # else{labels=res@labels}
  #
  # obsPrices <- res@prices
  # obsShares <- res@shares
  # obsMargins <- res@margins
  # obsElast <- res@mktElast
  #
  # #if(length(obsMargins[!is.na(obsMargins)]) < 2){return()}
  #
  # prePrices <- unname(drop(res@pricePre))
  # preMargins <- drop(calcMargins(res, preMerger=TRUE))
  # preShares <- drop(calcShares(res, preMerger=TRUE))
  # preShares <- drop(preShares/sum(preShares))
  # preElast <- elast(res, preMerger=TRUE, market=TRUE)

  diagnosticData <- calcDiagnostics(res)
  isVertical <- grepl("Vert", class(res))

  if (!mktElast) {

    #' table <- data.frame(
    #'   "Inputted Prices"= obsPrices,
    #'   "Fitted Prices" = prePrices,
    #'   "Price Change (%)"= (1 - obsPrices/prePrices)*100,
    #'   "Inputted Shares (%)" = obsShares*100,
    #'   "Fitted Shares(%)"=preShares*100,
    #'   "Share Change (%)"=(1 - obsShares/preShares)*100,
    #'   "Inputted Margins (%)" = obsMargins*100,
    #'   "Fitted  Margins (%)"= preMargins *100,
    #'   "Margin Change (%)"= (1 - obsMargins/preMargins)*100,
    #'   #'Market Elasticity'= 1 - obsElast/preElast,
    #'   check.names = FALSE
    #' )
    #'
    #' #rmThese <- colSums(abs(res),na.rm=TRUE)
    #' if(isCournot)  table[-1,grepl('Prices',colnames(table))] <- NA
    #'
    #' #res <- res[,rmThese >1e-3,drop=FALSE]
    #' if(!isCournot) rownames(table) <- labels

    table <- diagnosticData
    table$`Market Elasticity` <- NULL

    if (isVertical) {

      # Rename columns
      colnames(table)[colnames(table) == "upPrices"] <- "Upstream Price Change (%)"
      colnames(table)[colnames(table) == "downPrices"] <- "Downstream Price Change (%)"
      colnames(table)[colnames(table) == "Shares"] <- "Downstream Share Change (%)"
      colnames(table)[colnames(table) == "upMargins"] <- "Upstream Margin Change (%)"
      colnames(table)[colnames(table) == "downMargins"] <- "Downstream Margin Change (%)"

    } else {

      # Rename columns
      colnames(table)[colnames(table) == "Prices"] <- "Price Change (%)"
      colnames(table)[colnames(table) == "Shares"] <- "Share Change (%)"
      colnames(table)[colnames(table) == "Margins"] <- "Margin Change (%)"

    }

  } else {

    table <- data.frame(
              'Market Elasticity Change (%)' = unique(diagnosticData$`Market Elasticity`),
              check.names = FALSE)
  }

  return(table)
}
