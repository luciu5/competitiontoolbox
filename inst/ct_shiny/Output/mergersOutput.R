
## Display hot inputData
# Horizontal
output$hot <- renderRHandsontable({

  inputData <- values[["inputData"]]

  prices <- inputData[,"Prices \n($/unit)"]
  output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

  missPrices <- isTRUE(any(is.na(prices[!is.na(output)])))

  if(input$supply == "2nd Score Auction"){colnames(inputData)[grepl("Cost Changes",colnames(inputData))] <-'Post-merger\n Cost Changes\n($/unit)'}
  else{colnames(inputData)[grepl("Cost Changes",colnames(inputData))] <-'Post-merger\n Cost Changes\n(Proportion)'}

  if(missPrices && input$supply == "2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
  else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

  if (missPrices && any(grepl("ces|aids", demand(), perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
  else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}

  if (!is.null(inputData))
    rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
    hot_col(col = which(sapply(inputData, is.numeric)), halign = "htCenter") %>% hot_cols(columnSorting = TRUE)
})

# Vertical
output$hotVertical <- renderRHandsontable({

  inputData <- valuesVertical[["inputData"]]

  missPricesDown <- isTRUE(any(is.na(inputData$pricesDown[!is.na(inputData$sharesDown)])))
  missPricesUp <- isTRUE(any(is.na(inputData$pricesUp[!is.na(inputData$sharesDown)])))

  ### UNNECESSARY DUE TO VERTICAL.BARG() INPUT CONSTRAINTS, BUT KEEP FOR NOW (DISCUSS WITH CHARLES):
  if(missPricesDown && input$supplyVertical == "2nd Score Auction"){colnames(inputData)[grepl("marginsDown", colnames(inputData))] <- "marginsDown \n ($/unit)"}
  else{colnames(inputData)[grepl("marginsDown", colnames(inputData))] <- "marginsDown \n (p-c)/p"}

  if(missPricesUp && input$supplyVertical == "2nd Score Auction"){colnames(inputData)[grepl("marginsUp", colnames(inputData))] <- "marginsUp \n ($/unit)"}
  else{colnames(inputData)[grepl("marginsUp", colnames(inputData))] <- "marginsUp \n (p-c)/p"}

  if (!is.null(inputData))
    rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
    hot_col(col = which(sapply(inputData, is.numeric)), halign = "htCenter") %>% hot_cols(columnSorting = TRUE)
})



## Display summary results from mergersSummary
# Horizontal
output$results <-

  renderTable({

    if(input$inTabset != "respanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

    #inputData <- values[["inputData"]]
    mergersSummary(values[["sim"]])

  }, na = "", digits = 1)

# Vertical
output$resultsVertical <-

  renderTable({

    if(input$inTabsetVertical != "respanelVertical" || input$simulateVertical == 0 || is.null(valuesVertical[["sim"]])){return()}

    #inputData <- valuesVertical[["inputData"]]
    capture.output(result <- summary(valuesVertical[["sim"]], market = TRUE))
    result <- as.data.frame(result)

  }, na = "", digits = 1)



## Generate no-purchase shares in Details tab
# Horizontal
output$results_shareOut <- renderTable({

  if(input$inTabset != "detpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

  mergersNoPurch(values[["sim"]])

}, rownames = TRUE, digits = 1, align = "c")

# Vertical
output$results_shareOutVertical <- renderTable({

  if(input$inTabsetVertical != "detpanelVertical" || input$simulateVertical == 0  || is.null(valuesVertical[["sim"]])){return()}

  mergersNoPurch(valuesVertical[["sim"]])

}, rownames = TRUE, digits = 1, align = "c")



## Display detailed summary values to details tab
# Horizontal
output$results_detailed <- renderTable({

  if(input$inTabset != "detpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

  if(input$supply == "Cournot"){

    res <- NULL
    capture.output(try(res <- summary(values[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))
    res$product <- res$mcDelta <- NULL

    try(colnames(res) <- c("Merging Party","Name","Pre-Merger Price","Post-Merger Price", "Price Change (%)","Pre-Merger Quantity","Post-Merger Quantity", "Output Change (%)"),silent=TRUE)

  } else {

    isAuction <- grepl("Auction",class(values[["sim"]]))
    isRevDemand <- grepl("ces|aids",class(values[["sim"]]),ignore.case = TRUE)
    inLevels <- FALSE
    #isAIDS <- grepl("aids",class(values[["sim"]]),ignore.case = TRUE)
    missPrice <- any(is.na(values[["sim"]]@prices))
    if(isAuction && missPrice){inLevels = TRUE}

    capture.output(res <- summary(values[["sim"]], revenue=isRevDemand & missPrice, insideOnly=TRUE, levels=inLevels))
    res$Name <- rownames(res)
    res$mcDelta <- NULL
    res <- res[,c(1, ncol(res), 2 : (ncol(res)  - 1))]
    res$cmcr <- NA
    try(res$cmcr[res$isParty=="*"] <- cmcr(values[["sim"]]))

    thesenames <-  c("Merging Party","Name","Pre-Merger Price","Post-Merger Price", "Price Change (%)","Pre-Merger Share (%)","Post-Merger Share (%)", "Share Change (%)",'Compensating Marginal Cost Reduction (%)')

    #if(isAIDS && missPrice){thesenames <- thesenames[!thesenames %in% c("Pre-Merger Price","Post-Merger Price")]}
    colnames(res) <- thesenames

    if(all(is.na(res$`Compensating Marginal Cost Reduction (%)`))) res$`Compensating Marginal Cost Reduction (%)` <- NULL
    #res[,c("Pre-Merger Share (%)","Post-Merger Share (%)")] <-  res[,c("Pre-Merger Share (%)","Post-Merger Share (%)")] * 100 / colSums( res[,c("Pre-Merger Share (%)","Post-Merger Share (%)")])

    if(inLevels){ colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}

  }

  res

}, digits = 2)

# Vertical
output$results_detailedVertical <- renderTable({

  if(input$inTabsetVertical != "detpanelVertical" || input$simulateVertical == 0 || is.null(valuesVertical[["sim"]])){return()}

  ##
  ## ASK CHARLES if the complicated logic for results_detailed in -Horizontal- needs to be replicated here...
  ##

  capture.output(result <- summary(valuesVertical[["sim"]]))
  result <- as.data.frame(result)

  result$Name <- rownames(result)
  result <- result[, c(1, ncol(result), 2:(ncol(result)-1))]

}, digits = 2)



## Display market elasticity in Elasticities tab
# Horizontal
output$results_mktelast <- renderTable({

  if(input$inTabset != "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

  if(input$pre_elast == "Pre-Merger"){ preMerger = TRUE}
  else{preMerger = FALSE}

  res <- as.matrix(elast(values[["sim"]], preMerger=preMerger, market = TRUE))
  colnames(res) <- "Market"
  res

}, rownames = FALSE)

# Vertical
output$results_mktelastVertical <- renderTable({

  if(input$inTabsetVertical != "elastpanelVertical" || input$simulateVertical == 0 || is.null(valuesVertical[["sim"]])){return()}

  if(input$pre_elastVertical == "Pre-Merger"){preMerger = TRUE}
  else{preMerger = FALSE}

  result <- as.matrix(elast(valuesVertical[["sim"]], preMerger = preMerger, market = TRUE))
  colnames(res) <- "Market"
  res

}, rownames = FALSE)



## Display elasticities to Elasticities tab
# Horizontal
output$results_elast <- renderTable({

  if(input$inTabset!= "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

  isCournot <- grepl("Cournot",class(values[["sim"]]))

  if(input$pre_elast == "Pre-Merger"){ preMerger = TRUE}
  else{preMerger =FALSE}

  if(!isCournot && input$diversions){
    res <- diversion(values[["sim"]], preMerger=preMerger)
  }
  else{res <- elast(values[["sim"]], preMerger=preMerger)}
  if(isCournot){colnames(res) <- "Elasticity"}

  res

}, rownames = TRUE)

# Vertical



## Display inputted vs fitted mkt elasticity in Diagnostics tab
# Horizontal
output$results_diag_elast <- renderTable({

  if(input$inTabset != "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

  res <- mergersDiag(values[["sim"]], mktElast = TRUE)
  res

}, digits = 2, rownames = FALSE, align = "c")

# Vertical
output$results_diag_elastVertical <- renderTable({

  if(input$inTabsetVertical != "diagpanel" || input$simulateVertical == 0 || is.null(valuesVertical[["sim"]])){return()}

  ##
  ## UNCLEAR what to do here. Should we look at upstream or downstream margins/prices/etc...
  ##

  # res <- mergersDiag(valuesVertical[["sim"]], mktElast = TRUE)
  # res

}, digits = 2, rownames = FALSE, align = "c")


## Display diagnostic results to Diagnostics tab
# Horizontal
output$results_diagnostics <- renderTable({

  if(input$inTabset!= "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

  res <- mergersDiag(values[["sim"]])
  res

}, digits = 0 ,rownames = TRUE, align = "c")

# Vertical



## Identify whether the model is over-identified in Diagnostics tab
# Horizontal
output$overIDText <- renderText({

  if(is.null(values[["inputData"]])){return()}

  isOverID(input$supply, input$calcElast, values[["inputData"]])
})

# Vertical



## Display parameters to Diagnostics tab
# Horizontal
output$parameters <- renderPrint({

  if(input$inTabset!= "diagpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

  print(getParms(values[["sim"]], digits = 2))
})

# Vertical



## Display template code to the R Code tab
# Horizontal
output$results_code <- renderPrint({

  if(input$inTabset != "codepanel"){return()}

  thiscode <- mergersTemplateCode()
  cat(thiscode)
})

# Vertical



## Display warnings to Messages tab
# Horizontal
output$warnings <- renderText({

  if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]]$warning)){return()}

  paste(values[["msg"]]$warning,collapse="\n")
})

# Vertical



## Display errors to Messages tab
# Horizontal
output$errors <- renderText({

  if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]]$error)){cat(return())}

  paste(values[["msg"]]$error,collapse="\n")
})

# Vertical

