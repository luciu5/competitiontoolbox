mergersDiag <- function(res, mktElast = FALSE){
  ## calcDiagnostics is implemented by the upstream result class (including
  ## the vertical package's result classes); this wrapper only formats the
  ## table for the Shiny tabs.
  isVertical <- model_is_vertical(res)
  diagnosticData <- if (isVertical) {
    vertical::calcDiagnostics(res)
  } else {
    antitrust::calcDiagnostics(res)
  }

  if (!mktElast) {
    table <- diagnosticData
    table$`Market Elasticity` <- NULL

    if (isVertical) {

      colnames(table)[colnames(table) == "upPrices"] <- "Upstream Price Change (%)"
      colnames(table)[colnames(table) == "downPrices"] <- "Downstream Price Change (%)"
      colnames(table)[colnames(table) == "Shares"] <- "Downstream Share Change (%)"
      colnames(table)[colnames(table) == "upMargins"] <- "Upstream Margin Change (%)"
      colnames(table)[colnames(table) == "downMargins"] <- "Downstream Margin Change (%)"

    } else {

      colnames(table)[colnames(table) == "Prices"] <- "Price Change (%)"
      colnames(table)[colnames(table) == "Shares"] <- "Share Change (%)"
      colnames(table)[colnames(table) == "Margins"] <- "Margin Change (%)"

    }

  } else {

    table <- data.frame(
      'Market Elasticity Change (%)' = unique(diagnosticData$`Market Elasticity`),
      check.names = FALSE)
  }

  table
}
