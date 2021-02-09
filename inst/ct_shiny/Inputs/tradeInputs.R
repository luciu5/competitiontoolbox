
##### This function is just the "Tariffs"/"Quotas" version of genInputDataMergers() for "Horizontal". This entire thing needs to be desperately refactored...
tradeInputs <- function(nrows, type = c("Tariffs", "Quotas")) {
  # a function to generate default input data set for simulations

  type = match.arg(type)

  exampleData <- data.frame(
    Name = c("Prod1","Prod2","Prod3","Prod4"),
    Owner  = c("Firm1","Firm2","Firm3","Firm3"),
    'Prices \n($/unit)'    = rep(10,4),
    'Quantities'   =c(0.4,.3,.2,.1)*100,
    'Margins\n(p-c)/p' =c(0.25,NA,NA,NA),
    stringsAsFactors = FALSE,
    check.names=FALSE
  )

  exampleData <- exampleData[order(exampleData$`Quantities`, decreasing = TRUE),]
  rownames(exampleData) <- NULL

  if(type == "Tariffs"){
    fx <- data.frame('Current \nTariff \n(proportion)' = c(.05,.05,0,0),
                     'New \nTariff \n(proportion)' = c(.25,.25,0,0),
                     stringsAsFactors = FALSE,
                     check.names=FALSE)
  }
  else if (type == "Quotas"){
    fx <- data.frame('Current \nQuota \n(proportion)' = c(Inf,Inf,Inf,Inf),
                     'New \nQuota \n(proportion)' = c(.75,.75,Inf,Inf),
                     stringsAsFactors = FALSE,
                     check.names=FALSE)

  }


  exampleData <- cbind(exampleData, fx)

  inputData <- as.data.frame(matrix(NA_real_,nrow=max(nrows, nrow(exampleData)),ncol= ncol(exampleData)))
  colnames(inputData) <- colnames(exampleData)
  inputData[1:nrow(exampleData),] <- exampleData


  return(inputData)

}
