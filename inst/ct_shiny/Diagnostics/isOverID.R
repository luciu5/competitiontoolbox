
isOverID <- function(supply, calcElast, inputData){

  provElast <- grepl('elasticity', calcElast)

  nMargins <- inputData[, grepl("Margins", colnames(inputData))]
  nMargins <- length(nMargins[!is.na(nMargins)])

  if(supply == "Cournot" && ((provElast && nMargins > 0) || (!provElast && nMargins > 1))) {
    res <- paste(helpText(tags$b("Note:"), "Some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
  } else if(supply != "Cournot" && ((provElast && nMargins > 1) || (!provElast && nMargins > 2))) {
    res <- paste(helpText(tags$b("Note:"), "Some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
  } else {
    res <- paste(helpText(tags$b("Note:"), "Model parameters are just-identified. Inputted and fitted values should match."))
  }

  res
}
