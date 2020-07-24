
mergersInputs <- function(){
  # a function to generate default input data set for simulations

  inputData <- data.frame(
    Name = c("Prod1","Prod2","Prod3","Prod4"),
    'Pre-merger\n Owner'  = c("Firm1","Firm2","Firm3","Firm3"),
    'Post-merger\n Owner' = c("Firm1","Firm1","Firm3","Firm3"),
    'Prices \n($/unit)'    = rep(10,4),
    'Quantities'   = c(0.4,.3,.2,.1)*100,
    'Margins\n(p-c)/p' = c(0.5,NA,NA,NA),
    'Post-merger\n Cost Changes\n(Proportion)' = rep(0,4),
    stringsAsFactors = FALSE,
    check.names=FALSE
  )


  # if(req(input$menu) == "Horizontal"){
  #   if (req(supply()) == "Bertrand" & (req(demand()) == "logit (unknown elasticity)" | req(demand()) == "aids (unknown elasticity)")){
  #     inputData[['Margins\n(p-c)/p']] <- c(0.5, 0.5, NA, NA)
  #   }
  #   if (req(supply()) == "Bertrand" & req(demand()) == "ces (unknown elasticity)"){
  #     inputData[['Margins\n(p-c)/p']] <- c(0.5, 0.5, 0.25, 0.25)
  #   }
  # }


  nDefProd <- nrow(inputData)
  inputData <- inputData[c(1:nDefProd,rep(1, nPossProds - nDefProd)),]
  #if(input$incEff) inputData$mcDelta <- 0

  inputData[(nDefProd+1):nPossProds,] <- NA
  inputData <- inputData[order(inputData$`Quantities`, decreasing = TRUE),]
  rownames(inputData) <- NULL


  return(inputData)

}
