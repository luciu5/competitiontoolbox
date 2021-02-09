
## Display hot inputData
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


## Display summary results from tradeSummary
# Tariffs
output$resultsTariffs <-

  renderTable({

    if(input$inTabsetTariffs != "respanelTariffs" || input$simulateTariffs == 0|| is.null(valuesTariffs[["sim"]])){return()}

    # isolate(inputData <- valuesTariffs[["inputData"]])
    # tradeSummary(valuesTariffs[["sim"]], inputData, type = "Tariffs")
    capture.output(result <- summary(valuesTariffs[["sim"]], levels = FALSE, market = TRUE))
    result <- as.data.frame(result)

  })

# Quotas
output$resultsQuota <-

  renderTable({

    if(input$inTabsetQuota != "respanelQuota" || input$simulateQuota == 0|| is.null(valuesQuota[["sim"]])){return()}

    # isolate(inputData <- valuesQuota[["inputData"]])
    # tradeSummary(valuesQuota[["sim"]], inputData, type = "Quotas")
    capture.output(result <- summary(valuesQuota[["sim"]], levels = FALSE, market = TRUE))
    result <- as.data.frame(result)

  })


## Generate no-purchase shares in Details tab
# Tariffs
output$results_shareOutTariffs <- renderTable({

  if(input$inTabsetTariffs!= "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

  tradeNoPurch(valuesTariffs[["sim"]])

}, rownames = TRUE, digits = 1, align = "c")

# Quotas
output$results_shareOutQuota <- renderTable({

  if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

  tradeNoPurch(valuesQuota[["sim"]])

}, rownames = TRUE, digits = 1, align = "c")


## Display detailed summary values to details tab
# Tariffs
output$results_detailedTariffs <- renderTable({

  if(input$inTabsetTariffs != "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

  # isolate(inputData <- valuesTariffs[["inputData"]])
  # tradeSummary(valuesTariffs[["sim"]], inputData, type = "Tariffs")
  capture.output(result <- summary(valuesTariffs[["sim"]], levels = FALSE, market = FALSE))
  result <- as.data.frame(result)

})

# Quota
output$results_detailedQuota <- renderTable({

  if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

  # isolate(inputData <- valuesTariffs[["inputData"]])
  # tradeSummary(valuesTariffs[["sim"]], inputData, type = "Tariffs")
  capture.output(result <- summary(valuesQuota[["sim"]], levels = FALSE, market = FALSE))
  result <- as.data.frame(result)

})


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


## Identify whether the model is over-identified in Diagnostics tab
# Tariffs
output$overIDTextTariffs <- renderText({

  if(is.null(valuesTariffs[["inputData"]])){return()}

  isOverID(input$supplyTariffs, input$calcElastTariffs, valuesTariffs[["inputData"]])
})

# Quotas
output$overIDTextQuota <- renderText({

  if(is.null(valuesQuota[["inputData"]])){return()}

  isOverID(input$supplyQuota, input$calcElastQuota, valuesQuota[["inputData"]])
})


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
