
mergersInputs <- function(type = c("Horizontal", "Vertical")) {
  # a function to generate default input data set for simulations

  type = match.arg(type)

  if (type == "Horizontal") {

    inputData <- data.frame(
      Name = c("Prod1","Prod2","Prod3","Prod4"),
      'Pre-merger\n Owner' = c("Firm1","Firm2","Firm3","Firm3"),
      'Post-merger\n Owner' = c("Firm1","Firm1","Firm3","Firm3"),
      'Prices \n($/unit)' = rep(10,4),
      'Quantities' = c(0.4,.3,.2,.1)*100,
      'Margins\n(p-c)/p' = c(0.5,NA,NA,NA),
      'Post-merger\n Cost Changes\n(Proportion)' = rep(0,4),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # if(req(input$menu) == "Horizontal"){
    #   if (req(supply()) == "Bertrand" & (req(demand()) == "logit (unknown elasticity)" | req(demand()) == "aids (unknown elasticity)")){
    #     inputData[['Margins\n(p-c)/p']] <- c(0.5, 0.5, NA, NA)
    #   }
    #   if (req(supply()) == "Bertrand" & req(demand()) == "ces (unknown elasticity)"){
    #     inputData[['Margins\n(p-c)/p']] <- c(0.5, 0.5, 0.25, 0.25)
    #   }
    # }
  }

  if (type == "Vertical") {

    inputData <- data.frame(
      Name = c("Prod1","Prod2","Prod3","Prod4"),
      'ownerPreUp' = c("U1","U2","U1","U2"),
      'ownerPostUp' = c("U1","U1","U1","U1"),
      'pricesUp' = rep(10,4),
      'marginsUp' = rep(0.5,4),
      'ownerPreDown' = c("D1","D1","D2","D2"),
      'ownerPostDown' = c("D1","D1","D2","D2"),
      'pricesDown' = rep(20,4),
      'marginsDown' = rep(0.25,4),
      'sharesDown' = c(0.3, 0.3, 0.2, 0.1),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  nDefProd <- nrow(inputData)
  #inputData <- inputData[c(1:nDefProd,rep(1, nPossProds - nDefProd)),]
  #if(input$incEff) inputData$mcDelta <- 0

  inputData[(nDefProd + 1):nPossProds, ] <- NA
  #inputData <- inputData[order(inputData$`Quantities`, decreasing = TRUE),]
  #rownames(inputData) <- NULL

  return(inputData)

}
