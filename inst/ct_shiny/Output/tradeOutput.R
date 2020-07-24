
## Display inputData
# Tariffs
output$hotTariffs <- renderRHandsontable({

  inputData <- valuesTariffs[["inputData"]]

  colnames(inputData) <- gsub("Quota","Tariff", colnames(inputData))

  prices <- inputData[,"Prices \n($/unit)"]
  output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

  missPrices <- isTRUE(any(is.na(prices[ !is.na(output) ] ) ))

  if(input$supplyTariffs == "2nd Score Auction"){ colnames(inputData) <- gsub("Tariff \n(proportion)","Tariff \n($/unit)", colnames(inputData))}
  else{colnames(inputData) <- gsub("Tariff \n($/unit)","Tariff \n(proportion)", colnames(inputData))}

  if(missPrices && input$supplyTariffs =="2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
  else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

  if (missPrices && any(grepl("ces|aids", demand(), perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
  else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}

  if (!is.null(inputData))
    rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
    hot_col(col = which (sapply(inputData,is.numeric)),halign = "htCenter" ) %>% hot_cols(columnSorting = TRUE)
})

# Quotas
output$hotQuota <- renderRHandsontable({

  inputData <- valuesQuota[["inputData"]]

  colnames(inputData) <- gsub("Tariff","Quota", colnames(inputData))

  prices <- inputData[,"Prices \n($/unit)"]
  output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

  missPrices <- isTRUE(any(is.na(prices[ !is.na(output) ] ) ))

  if(input$supplyQuota == "2nd Score Auction"){ colnames(inputData) <- gsub("Quota \n(proportion)","Quota \n($/unit)", colnames(inputData))}
  else{colnames(inputData) <- gsub("Quota \n($/unit)","Quota \n(proportion)", colnames(inputData))}

  if(missPrices && input$supplyQuota =="2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
  else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

  if (missPrices && any(grepl("ces|aids", demand(), perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
  else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}

  if (!is.null(inputData))
    rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
    hot_col(col = which (sapply(inputData,is.numeric)),halign = "htCenter" ) %>% hot_cols(columnSorting = TRUE)
})


## Display summary results from tradeSummary to results tab
# Tariffs
output$resultsTariffs <-

  renderTable({

    if(input$inTabsetTariffs != "respanelTariffs" || input$simulateTariffs == 0|| is.null(valuesTariffs[["sim"]])){return()}

    isolate(inputData <- valuesTariffs[["inputData"]])
    tradeSummary(valuesTariffs[["sim"]], inputData, type = "Tariffs")

  })

# Quotas
output$resultsQuota <-

  renderTable({

    if(input$inTabsetQuota != "respanelQuota" || input$simulateQuota == 0|| is.null(valuesQuota[["sim"]])){return()}

    isolate(inputData <- valuesQuota[["inputData"]])
    tradeSummary(valuesQuota[["sim"]], inputData, type = "Quotas")

  })


## Display detailed summary values to details tab
# Tariffs
output$results_detailedTariffs <- renderTable({

  if(input$inTabsetTariffs != "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

  if(input$supplyTariffs == "Cournot"){

    res <- NULL
    capture.output(try(res <- summary(valuesTariffs[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))

    res$isParty <- factor(res$mcDelta >0, labels=c("","*"))
    res$product <- res$mcDelta <- NULL
    try(colnames(res) <- c("Foreign Firm","Name","Current Tariff Price","New Tariff Price", "Price Change (%)","Current Tariff Quantity","New Tariff Quantity", "Output Change (%)"),silent=TRUE)

  } else {

    isAuction <- grepl("Auction",class(valuesTariffs[["sim"]]))
    isRevDemand <- grepl("ces|aids",class(valuesTariffs[["sim"]]),ignore.case = TRUE)
    inLevels <- FALSE
    #isAIDS <- grepl("aids",class(values[["sim"]]),ignore.case = TRUE)
    missPrice <- any(is.na(valuesTariffs[["sim"]]@prices))

    if(isAuction && missPrice){inLevels = TRUE}

    capture.output(res <- summary(valuesTariffs[["sim"]], revenue=isRevDemand & missPrice, insideOnly=TRUE, levels=inLevels))
    res$Name <- rownames(res)
    res$isParty <- factor(res$mcDelta >0, labels=c("","*"))
    res$mcDelta <- NULL
    res <- res[,c(1, ncol(res), 2 : (ncol(res)  - 1))]

    thesenames <-  c("Foreign Firm","Name","Current Tariff Price","New Tariff Price", "Price Change (%)","Current Tariff Share (%)","New Tariff Share (%)", "Share Change (%)")
    colnames(res) <- thesenames

    if(inLevels){colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}

  }

  res

}, digits = 2)

# Quota
output$results_detailedQuota <- renderTable({

  if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

  if(input$supplyQuota == "Cournot"){

    res <- NULL
    capture.output(try(res <- summary(valuesQuota[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))

    res$isParty <- factor(is.finite(valuesQuota[["sim"]]@capacitiesPre) | is.finite(valuesQuota[["sim"]]@capacitiesPost), labels=c("","*"))
    res$product <- res$mcDelta <- NULL
    try(colnames(res) <- c("Foreign Firm","Name","Current Quota Price","New Quota Price", "Price Change (%)","Current Quota Quantity","New Quota Quantity", "Output Change (%)"),silent=TRUE)

  } else {

    isAuction <- grepl("Auction",class(valuesQuota[["sim"]]))
    isRevDemand <- grepl("ces|aids",class(valuesQuota[["sim"]]),ignore.case = TRUE)
    inLevels <- FALSE
    #isAIDS <- grepl("aids",class(valuesQuota[["sim"]]),ignore.case = TRUE)
    missPrice <- any(is.na(valuesQuota[["sim"]]@prices))
    if(isAuction && missPrice){inLevels = TRUE}

    capture.output(res <- summary(valuesQuota[["sim"]], revenue=isRevDemand & missPrice, insideOnly=TRUE, levels=inLevels))
    res$Name <- rownames(res)

    res$isParty <- factor(is.finite(valuesQuota[["sim"]]@capacitiesPre) | is.finite(valuesQuota[["sim"]]@capacitiesPost), labels=c("","*"))

    res$mcDelta <- NULL
    res <- res[,c(1, ncol(res), 2 : (ncol(res)  - 1))]

    thesenames <-  c("Foreign Firm","Name","Current Quota Price","New Quota Price", "Price Change (%)","Current Quota Share (%)","New Quota Share (%)", "Share Change (%)")
    colnames(res) <- thesenames

    if(inLevels){colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}
  }

  res

}, digits = 2)


## Display market elasticity in Elasticities tab
# Tariffs
output$results_mktelastTariffs <- renderTable({

  if(input$inTabsetTariffs!= "elastpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

  if(input$pre_elast == "Current Tariff"){preTariff = TRUE}
  else{preTariff = FALSE}

  res <- as.matrix(elast(valuesTariffs[["sim"]], preMerger=preTariff, market = TRUE))
  colnames(res)= "Market"
  res

}, rownames = FALSE)

# Quotas
output$results_mktelastQuota <- renderTable({

  if(input$inTabsetQuota!= "elastpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

  if(input$pre_elast == "Current Quota"){ preQuota = TRUE}
  else{preQuota = FALSE}

  res <- as.matrix(elast(valuesQuota[["sim"]], preMerger=preQuota, market = TRUE))
  colnames(res)= "Market"
  res

}, rownames = FALSE)


## Display elasticities to Elasticities tab
# Tariffs
output$results_elastTariffs <- renderTable({

  if(input$inTabsetTariffs!= "elastpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

  isCournot <- grepl("Cournot",class(valuesTariffs[["sim"]]))

  if(input$pre_elastTariffs == "Current Tariff"){preMerger = TRUE}
  else{preMerger = FALSE}

  if(!isCournot && input$diversionsTariffs){
    res <- diversion(valuesTariffs[["sim"]], preMerger=preMerger)
  }
  else{  res <- elast(valuesTariffs[["sim"]], preMerger=preMerger)}
  if(isCournot){colnames(res) <- "Elasticity"}

  res

}, rownames = TRUE)

# Quotas
output$results_elastQuota <- renderTable({

  if(input$inTabsetQuota != "elastpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

  isCournot <- grepl("Cournot",class(valuesQuota[["sim"]]))

  if(input$pre_elastQuota == "Current Quota"){ preMerger = TRUE}
  else{preMerger =FALSE}

  if(!isCournot && input$diversionsQuota){
    res <- diversion(valuesQuota[["sim"]], preMerger=preMerger)
  }
  else{  res <- elast(valuesQuota[["sim"]], preMerger=preMerger)}
  if(isCournot){colnames(res) <- "Elasticity"}

  res

}, rownames = TRUE)


## Display market elasticity gap in Diagnostics tab
# Tariffs
output$results_diag_elastTariffs <- renderTable({

  if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

  res <- tradeDiag(valuesTariffs[["sim"]], mktElast=TRUE)
  res

}, digits = 2, rownames = FALSE, align = "c")

# Quotas
output$results_diag_elastQuota <- renderTable({

  if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

  res <- tradeDiag(valuesQuota[["sim"]], mktElast=TRUE)
  res

}, digits = 2, rownames = FALSE, align = "c")


## Display results to Diagnostics tab
# Tariffs
output$results_diagnosticsTariffs <- renderTable({

  if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

  res <- tradeDiag(valuesTariffs[["sim"]])
  res

}, digits = 0 ,rownames = TRUE, align = "c")

# Quotas
output$results_diagnosticsQuota <- renderTable({

  if(input$inTabsetQuota != "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

  res <- tradeDiag(valuesQuota[["sim"]])
  res

}, digits = 0 ,rownames = TRUE, align = "c")


## Display parameters to Diagnostics tab
# Tariffs
output$parametersTariffs <- renderPrint({

  if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

  print(getParms(valuesTariffs[["sim"]],digits=2))

})

# Quotas
output$parametersQuota <- renderPrint({

  if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

  print(getParms(valuesQuota[["sim"]], digits = 2))

})


## Display template code to the R Code tab
# Tariffs
output$results_codeTariffs <- renderPrint({

  if(input$inTabsetTariffs != "codepanelTariffs"){return()}

  thiscode <- tradeTemplateCode("Tariffs")
  cat(thiscode)
})

# Quotas
output$results_codeQuota <- renderPrint({

  if(input$inTabsetQuota != "codepanelQuota"){return()}

  thiscode <- tradeTemplateCode("Quotas")
  cat(thiscode)
})


## Display warnings to Messages tab
# Tariffs
output$warningsTariffs <- renderPrint({

  if(input$inTabsetTariffs!= "msgpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["msg"]])){return()}

  print(valuesTariffs[["msg"]]$warning)
})

# Quotas
output$warningsQuota <- renderPrint({

  if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]])){return()}

  print(valuesQuota[["msg"]]$warning)
})


## Display errors to Messages tab
# Tariffs
output$errorsTariffs <- renderPrint({

  if(input$inTabsetTariffs!= "msgpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["msg"]]$error)){cat(return())}

  print(valuesTariffs[["msg"]]$error)
})

# Quotas
output$errorsQuota <- renderPrint({

  if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]]$error)){cat(return())}

  print(valuesQuota[["msg"]]$error)
})
