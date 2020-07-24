
get_vignette_link <- function(...) {
    x <- vignette(...)
    if (nzchar(out <- x$PDF)) {
        ext <- tools::file_ext(out)
        port <- if (tolower(ext) == "html")
            tools::startDynamicHelp(NA)
        else 0L
        if (port > 0L) {
            out <- sprintf("http://127.0.0.1:%d/library/%s/doc/%s",
                           port, basename(x$Dir), out)
            return(out)
        }
    }
    stop("no html help found")
}

data("indicboxdata", package = "competitiontoolbox")
data("sumboxdata", package = "competitiontoolbox")
data("indicboxmktCnt", package = "competitiontoolbox")
data("sumboxmktCnt", package = "competitiontoolbox")

shinyServer(function(input, output, session) {

    nPossProds <- 10  # Only allow 10 products by default

    msgCatcher <- function(expr){

            W <- NULL
            E <- NULL

            w.handler <- function(w){ # warning handler
                W <<- append(W,conditionMessage(w))
                invokeRestart("muffleWarning")
            }

            e.handler <- function(e){ # error handler
                E <<- append(E, conditionMessage(e))
                NULL
            }

            list(value = withCallingHandlers(tryCatch(expr, error = e.handler),
                                             warning = w.handler),
                 warning = W, error = E)
        }


    isOverID <-  function(supply, calcElast, inputData){

        provElast <- grepl('elasticity',calcElast)

        nMargins <- inputData[,grepl("Margins",colnames(inputData))]
        nMargins <- length(nMargins[!is.na(nMargins)])

        if(supply == "Cournot" &&
           ((provElast && nMargins > 0)  || (!provElast && nMargins >1) )){
            res <- paste(helpText(tags$b("Note:"), "some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
        } else if(supply != "Cournot" &&
                ((provElast && nMargins > 1)  || (!provElast && nMargins >2) )){
            res <- paste(helpText(tags$b("Note:"),"some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
        } else {
            res <- paste(helpText(tags$b("Note:"),"model parameters are just-identified. Inputted and fitted values should match."))
        }

        res
    }


    genShareOut <- function(sim){

      if( grepl("cournot",class(sim),ignore.case = TRUE)){return()}

      isCES <- grepl("ces",class(sim),ignore.case = TRUE)

      res <- data.frame('No-purchase\n Share (%)'= c(
        1 - sum(calcShares(sim, preMerger=TRUE,revenue=isCES)),
        1 - sum(calcShares(sim, preMerger=FALSE,revenue=isCES))
      )
      ,check.names = FALSE
      )*100

      res$'Revenues ($)' <- as.integer(round(c(calcRevenues(sim, preMerger=TRUE, market = TRUE ),
                                               calcRevenues(sim, preMerger=FALSE, market = TRUE )
      )))

      rownames(res) <- c("Current Tariff","New Tariff")

      if( grepl("aids",class(sim),ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

      return(res)
    }


    ##### Source Mergers and Trade functions
    # Inputs
    source(paste0(getwd(), "/Inputs/mergersInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/tradeInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/reactiveInputs.R"), local = TRUE)

    # Simulations
    source(paste0(getwd(), "/Simulations/mergersSims.R"), local = TRUE)
    source(paste0(getwd(), "/Simulations/tradeSims.R"), local = TRUE)

    # Summary Tables
    source(paste0(getwd(), "/Summary Tables/mergersSummary.R"), local = TRUE)
    source(paste0(getwd(), "/Summary Tables/tradeSummary.R"), local = TRUE)

    # Diagnostics Data
    source(paste0(getwd(), "/Diagnostics Data/mergersDiag.R"), local = TRUE)
    source(paste0(getwd(), "/Diagnostics Data/tradeDiag.R"), local = TRUE)

    # Template Code
    source(paste0(getwd(), "/Template Code/mergersTemplateCode.R"), local = TRUE)
    source(paste0(getwd(), "/Template Code/tradeTemplateCode.R"), local = TRUE)

    # Output



    # Creates the graph for the Summary tab of Numerical Simulations
    output$plotSumATR <- renderPlot({

        if(grepl("%",input$outcomeSumATR)) ylimSumATR <- c(0,50)
        else{ylimSumATR <- c(0,350)}

        ggplot(data = subset(sumboxdata, Outcome == input$outcomeSumATR & shareOutThresh == input$shareOutSumATR), aes(x=Model, ymin=low_wisk,lower=pct25,middle=pct50,upper=pct75,ymax=high_wisk))+
          geom_boxplot(stat = "identity", lwd = 0.75, fatten = 1) +
          coord_cartesian(ylim = ylimSumATR)+
          theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title = element_text(size=13), axis.text.x  = element_text(angle =45 , hjust=1, size=11, face = "bold")) +
          ylab(input$outcomeSumATR) +
          ggtitle(paste0(input$outcomeSumATR, ", Outside Share Less Than ", input$shareOutSumATR,"%"))

    })

    # Creates the graph for the Indices tab of Numerical Simulations
    output$plotIndATR <- renderPlot({

        plotInd <- ggplot(subset(indicboxdata, Cut_type == input$indexIndATR & Supply == "Pooled" & shareOutThresh == input$shareOutIndATR),
                          aes(x=Cut_value,ymin=low_wisk,lower=pct25,middle=pct50,upper=pct75,ymax=high_wisk)) +
            geom_boxplot(stat = "identity", lwd = 0.75, fatten = 1) +
            coord_cartesian(ylim = c(0,30)) + theme_bw() +
            xlab(input$indexIndATR) + ylab("Industry Price Change (%)") +
            theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text.y = element_text(size=11), axis.title = element_text(size=13), axis.text.x = element_text(angle = 45,hjust = 1, size = 12)) +
            geom_hline(yintercept=0, col = "#d95f02",linetype = "dashed") +
            geom_hline(yintercept=c(1,5,10),linetype="dashed") +
            ggtitle(paste0(input$indexIndATR,", Outside Share Less Than ",input$shareOutIndATR,"% (",input$pooledIndATR,")"))

        plot(plotInd)

        if (input$pooledIndATR == "By Demand Model") {
            plotInd %+% subset(indicboxdata, Cut_type == input$indexIndATR & shareOutThresh == input$shareOutIndATR & !Supply == "Pooled") +
                facet_wrap(Supply~Demand,scales = "fixed",nrow=1) + theme(axis.text.y  = element_text(size=7))
        }
    })

    # Number of Simulated Mergers for Indices and Summary Tab of ATR Numerical Simulations
    output$sumNumMergerATR <- renderUI({
        sumNumMerg <- subset(sumboxmktCnt, Outcome == input$outcomeSumATR & shareOutThresh == input$shareOutSumATR)
        sumNumMerg <- prettyNum(sum(sumNumMerg$Cnt), big.mark=",")
        HTML(paste("Examine the distribution of outcomes from", sumNumMerg, "simulated horizontal mergers."))
    })

    output$indicNumMergerATR <- renderUI({
        indicNumMerg <- prettyNum(indicboxmktCnt$Cnt[which(indicboxmktCnt$Cut_type == input$indexIndATR & indicboxmktCnt$shareOutThresh == input$shareOutIndATR )], big.mark=",")
        HTML(paste("Examine the relationship between industry price changes and commmonly used merger indices from", prettyNum((indicNumMerg), big.mark=","), "simulated horizontal mergers."))
    })

    #Generates Captions for Summary and Indices Graphs of ATR Numerical Simulations
    output$capSumATR <- renderText({
        captionSumATR()
    })

    captionSumATR <- reactive({
        switch(input$outcomeSumATR, 'Consumer Harm ($)' = "Consumer Harm Measured in Dollars.",
               'Producer Benefit ($)' = "Producer Benefit Measured in Dollars.",
               'Net Harm ($)' = "Net Harm Caused to Consumer Measured in Dollars.",
               'Industry Price Change (%)' = "Industry Price Change Measured as a Percent Change from Pre-Merger Price",
               'Merging Party Price Change (%)' = "Merging Party Price Change as a Percent Change from Pre-Merger Price")
    })

    output$capIndATR <- renderText({
        captionIndATR()
    })

    captionIndATR <-reactive({
        switch(input$indexIndATR, 'UPP' = "Upward Pricing Pressure (UPP): Describes the relationship between UPP and industry-wide price changes across the different models, confirming previous findings
             that UPP's predictive power substantially degrades as demand curvature increases. Second, with the exception of Cournot-log and Bertrand-AIDS specifications,
             UPP tends to over-predict the price effects from a merger",
               'HHI' = "Post-Merger Herfindahl-Hirschman Index (HHI): Demonstrates the distribution of estimated industry price changes by the level of the post-merger HHI for each model.
             Across all models, both the level and the variance of the quantiles of the estimated industry price changes increase with the change post-merger HHI.",
               'Delta HHI' = "Change in Post-Merger Herfindahl-Hirschman Index (Delta HHI): Presents the distribution of estimated industry price changes by the change in post-merger HHI for each model.
            Presents nearly identical results to 'HHI' with the added benefit that the change in the post-merger HHI is associated with a stronger negative correlation with a negligible
            price effect.",
               'CMCR' =  "Compensating Marginal Cost Reductions (CMCR): Displays the relationship between the Bertrand CMCR and industry-wide price changes across the different models.
            The range of price effects increases as the CMCR increases, with models having substantial demand curvature (i.e. Cournot, with Log-linear demand and Bertrand with AIDS demand)
            experiencing the largest increases in price dispersion.",
               'Firm Count' = "Number of Pre-Merger Firms (Firm Count): Depicts the distribution of industry price changes by Firm Count. Across all models, markets with fewer firms have exponentially larger average effects
            on industry-wide prices than markets with more firms.",
               'Party Gap' = "Difference in the Party Rank of the Merging Parties (Party Gap):  Presents Party Gap versus changes in industry-wide prices. As the merging parties grow closer in rank both the average price
            effects and the variance of the predicted price effects increase.",
               'Harm2nd' = "2nd Harm: Coming Soon")

    })

    output$referenceATR <-
        renderUI({
            tags$iframe(id = "referenceATR",
                        src = get_vignette_link("Reference", package = "antitrust"),
                        width = "100%",
                        height = 768,
                        frameborder = 0,
                        marginheight = 0
            )
        })

    output$referenceTrade <-
        renderUI({
            tags$iframe(id = "referenceTrade",
                        src = get_vignette_link("Reference", package = "trade"),
                        width = "100%",
                        height = 768,
                        frameborder = 0,
                        marginheight = 0
            )
        })


    ## create a reactive list of objects
    valuesQuota <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    valuesTariffs <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    values <- reactiveValues(inputData = mergersInputs(), sim = NULL, msg = NULL)


    ##### Initialize inputData (fix???)
    observeEvent((input$menu == "Tariffs") | input$addRowsTariffs,{
        valuesTariffs[["inputData"]] <- tradeInputs(nrow = input$addRowsTariffs, type = "Tariffs")}
    )

    observeEvent((input$menu == "Quotas") | input$addRowsQuota,{
        valuesQuota[["inputData"]]<- tradeInputs(nrow = input$addRowsQuota, type = "Quotas")

    })

    observeEvent(input$menu == "Merger",{
        values[["inputData"]] <- mergersInputs()
    })


    observe({

        if(!is.null(input$hotTariffs)){
            valuesTariffs[["inputData"]] = hot_to_r(input$hotTariffs)
        }

        if(!is.null(input$hotQuota)){
            valuesQuota[["inputData"]] = hot_to_r(input$hotQuota)
        }

        if(!is.null(input$hot)){
            values[["inputData"]] <- mergersInputs()

            values$inputData = hot_to_r(input$hot)
            values$inputData[!is.na(values$inputData$Name) & values$inputData$Name != '',]
        }
    })


    ## display inputs
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

    output$hot <- renderRHandsontable({

        inputData <- values[["inputData"]]

        prices <- inputData[,"Prices \n($/unit)"]
        output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

        missPrices <- isTRUE(any(is.na(prices[ !is.na(output) ] ) ))

        if(input$supply == "2nd Score Auction"){colnames(inputData)[grepl("Cost Changes",colnames(inputData))] <-'Post-merger\n Cost Changes\n($/unit)'}
        else{colnames(inputData)[grepl("Cost Changes",colnames(inputData))] <-'Post-merger\n Cost Changes\n(Proportion)'}

        if(missPrices && input$supply =="2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
        else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

        if (missPrices && any(grepl("ces|aids", demand(), perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
        else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}

        if (!is.null(inputData))
            rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
            hot_col(col = which (sapply(inputData,is.numeric)),halign = "htCenter" ) %>% hot_cols(columnSorting = TRUE)
    })



    ## simulate merger when the "simulate" button is clicked
    observeEvent(input$simulateTariffs | input$simulateQuota | input$simulate, {

        if(input$menu == "Tariffs"){

            valuesTariffs[["sim"]] <- valuesTariffs[["msg"]] <-  NULL
            updateTabsetPanel(session,inputId  = "inTabsetTariffs", selected = "respanelTariffs")
            indata <- valuesTariffs[["inputData"]]

        }
        else if (input$menu == "Quotas"){

            valuesQuota[["sim"]] <- valuesQuota[["msg"]] <-  NULL
            updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "respanelQuota")
            indata <-   valuesQuota[["inputData"]]

        } else{

            values[["sim"]] <- values[["msg"]] <-  NULL
            updateTabsetPanel(session,inputId  = "inTabset", selected = "respanel")
            indata <- values[["inputData"]]
        }

        isOutput <-  grepl("Quantities|Revenues",colnames(indata),perl = TRUE)

        colnames(indata)[ grepl("Margins",colnames(indata),perl = TRUE)] <- "Margins"
        colnames(indata)[isOutput] <- "Output"

        indata <- indata[!is.na(indata[,"Output"]),]
        indata$mcDelta <- 0

        if(input$menu == "Tariffs"){
            tariffPost <- indata[,grep('New.*\\n(Tariff)',colnames(indata), perl=TRUE), drop = TRUE]
            tariffPost[is.na(tariffPost)] <- 0

            tariffPre <- indata[,grep('Cur.*\\n(Tariff)',colnames(indata), perl = TRUE), drop= TRUE]
            tariffPre[is.na(tariffPre)] <- 0
            indata$mcDelta <-  (tariffPost - tariffPre)
            indata$mcDelta <-  indata$mcDelta/(1 - tariffPost)
        }

        #####
        if (req(input$menu) == "Tariffs") {
            indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

            ## Very useful, output the three information types for the merger simulation
            print(c(supply(), demand(), as.character(elasticity())))

            thisSim <- msgCatcher(
              # Run merger simulation
              tradeSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity(),
                        type = input$menu)
              )

        } else if (input$menu == "Quotas") {
            indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

            ## Very useful, output the three information types for the merger simulation
            print(c(supply(), demand(), as.character(elasticity())))

            thisSim <- msgCatcher(
              # Run merger simulation
              tradeSims(supply = supply(),demand = demand(), indata = indata, mktElast = elasticity(),
                      type = input$menu)
              )

        } else {

            indata$mcDelta <- indata[,grep('Cost Changes',colnames(indata))]
            indata$mcDelta[is.na(indata$mcDelta)] <- 0

            indata$'Pre-merger\n Owner' <- factor(indata$'Pre-merger\n Owner',levels=unique(indata$'Pre-merger\n Owner') )
            indata$'Post-merger\n Owner' <- factor(indata$'Post-merger\n Owner',levels=unique(indata$'Post-merger\n Owner'))

            ## Very useful, output the three information types for the merger simulation
            print(c(supply(), demand(), as.character(elasticity())))

            thisSim <- msgCatcher(
                # Run merger simulation
                mergersSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity())

                )

            thisSim <<- thisSim  # Delete later...
        }


        thisSim$warning <- grep("are the same|INCREASE in marginal costs", thisSim$warning, value= TRUE, invert=TRUE, perl=TRUE)
        if(length(thisSim$warning) == 0){thisSim$warning = NULL}

        if(input$menu == "Tariffs"){
            valuesTariffs[["sim"]]<-  thisSim$value
            valuesTariffs[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
        }
        else if(input$menu =="Quotas"){
            valuesQuota[["sim"]]<-  thisSim$value
            valuesQuota[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
        } else{
            values[["sim"]] <-  thisSim$value

            values[["msg"]] <-  list(error=thisSim$error,warning=thisSim$warning)
        }

        if(!is.null(thisSim$error) || !is.null(thisSim$warning)){

            if(input$menu == "Tariffs" ){
                updateTabsetPanel(session,inputId  = "inTabsetTariffs", selected = "msgpanelTariffs")
            }

            else  if(input$menu == "Quotas" ){
                updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "msgpanelQuota")
            } else if(input$menu == "Horizontal" ){
                updateTabsetPanel(session,inputId  = "inTabset", selected = "msgpanel")
            }
        }
    })


    ## identify whether model is over-identified
    output$overIDTextTariffs <-   renderText({

        if(is.null(valuesTariffs[["inputData"]])){return()}

        isOverID(input$supplyTariffs, input$calcElastTariffs, valuesTariffs[["InputData"]])
    })

    output$overIDTextQuota <-  renderText({

        if(is.null(valuesQuota[["inputData"]])){return()}

        isOverID(input$supplyQuota, input$calcElastQuota, valuesQuota[["InputData"]])
    })

    ## identify interface as Public Site or not
    output$overIDText <-   renderText({

        if(is.null(values[["inputData"]])){return()}

        provElast <- grepl('elasticity',input$calcElast)

        inputData <- values[["inputData"]]

        nMargins <- inputData[,grepl("Margins",colnames(inputData))]
        nMargins <- length(nMargins[!is.na(nMargins)])

        if(input$supply == "Cournot" &&
           ((provElast && nMargins > 0)  || (!provElast && nMargins >1) )){
            res <- paste(helpText(tags$b("Note:"), "some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
        }

        else if(input$supply != "Cournot" &&
                ((provElast && nMargins > 1)  || (!provElast && nMargins >2) )){
            res <- paste(helpText(tags$b("Note:"),"some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
        }
        else{
            res <- paste(helpText(tags$b("Note:"),"model parameters are just-identified. Inputted and fitted values should match."))

        }
        res

    })


    ## identify interface as Public Site or not
    output$urlTextQuota <-   output$urlTextTariffs <-   output$urlText <-   renderText({

        thisurl <- session$clientData$url_hostname

        if(grepl("atrnet\\.gov", thisurl, perl=TRUE)){
            res <- paste(h4(span("Internal Server",style="color:blue")))
        }
        else if(grepl("^127", thisurl, perl=TRUE)){
            res <- paste(h4(span("Local Server",style="color:green")))
        }
        else{
            res <- paste(h4(span("Public Server",style="color:red")))
        }

        res
    })


    ## display summary results from tradeSummary to results tab
    output$resultsTariffs <-

        renderTable({

            if(input$inTabsetTariffs != "respanelTariffs" || input$simulateTariffs == 0|| is.null(valuesTariffs[["sim"]])){return()}

            isolate(inputData <- valuesTariffs[["inputData"]])

            tradeSummary(valuesTariffs[["sim"]],inputData,type = "Tariffs")

        })

    output$resultsQuota <-

        renderTable({

            if(input$inTabsetQuota != "respanelQuota" || input$simulateQuota == 0|| is.null(valuesQuota[["sim"]])){return()}

            isolate(inputData <- valuesQuota[["inputData"]])

            tradeSummary(valuesQuota[["sim"]],inputData,type = "Quotas")

        })

    ## display summary results from mergersSummary to results tab
    output$results <-

        renderTable({

            if(input$inTabset != "respanel" || input$simulate == 0|| is.null(values[["sim"]])){return()}

            inputData <- values[["inputData"]]

            mergersSummary(values[["sim"]])

        }, na="", digits =1)


    ## display summary values to details tab
    output$results_detailedTariffs <- renderTable({

        if(input$inTabsetTariffs!= "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

        if(input$supplyTariffs == "Cournot"){

            res <- NULL
            capture.output(try(res <- summary(valuesTariffs[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))

            res$isParty <- factor(res$mcDelta >0, labels=c("","*"))

            res$product <- res$mcDelta <- NULL

            try(colnames(res) <- c("Foreign Firm","Name","Current Tariff Price","New Tariff Price", "Price Change (%)","Current Tariff Quantity","New Tariff Quantity", "Output Change (%)"),silent=TRUE)

        }

        else{

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

            if(inLevels){ colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}

        }

        res

    }, digits = 2)


    ## display summary values to details tab
    output$results_detailedQuota <- renderTable({

        if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

        if(input$supplyQuota == "Cournot"){

            res <- NULL
            capture.output(try(res <- summary(valuesQuota[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))


            res$isParty <- factor(is.finite(valuesQuota[["sim"]]@capacitiesPre) | is.finite(valuesQuota[["sim"]]@capacitiesPost), labels=c("","*"))
            res$product <- res$mcDelta <- NULL

            try(colnames(res) <- c("Foreign Firm","Name","Current Quota Price","New Quota Price", "Price Change (%)","Current Quota Quantity","New Quota Quantity", "Output Change (%)"),silent=TRUE)
        }

        else{

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

            if(inLevels){ colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}
        }

        res

    }, digits = 2)

    ## display summary values to details tab
    output$results_detailed <- renderTable({

        if(input$inTabset!= "detpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

        if(input$supply == "Cournot"){

            res <- NULL
            capture.output(try(res <- summary(values[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))
            res$product <- res$mcDelta <- NULL

            try(colnames(res) <- c("Merging Party","Name","Pre-Merger Price","Post-Merger Price", "Price Change (%)","Pre-Merger Quantity","Post-Merger Quantity", "Output Change (%)"),silent=TRUE)
        }

        else{

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


    output$results_shareOutTariffs <- renderTable({

        if(input$inTabsetTariffs!= "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

        genShareOut(valuesTariffs[["sim"]])

    }, rownames = TRUE, digits=1,align="c")


    output$results_shareOutQuota <- renderTable({

        if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

        genShareOut(valuesQuota[["sim"]])

    }, rownames = TRUE, digits=1,align="c")

    output$results_shareOut <- renderTable({

        if(input$inTabset!= "detpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}
        if( grepl("cournot",class(values[["sim"]]),ignore.case = TRUE)){return()}

        isCES <- grepl("ces",class(values[["sim"]]),ignore.case = TRUE)

        res <- data.frame('No-purchase\n Share (%)'= c(
            1 - sum(calcShares(values[["sim"]], preMerger=TRUE,revenue=isCES)),
            1 - sum(calcShares(values[["sim"]], preMerger=FALSE,revenue=isCES))
        )
        ,check.names = FALSE
        )*100

        res$'Revenues ($)' <- as.integer(round(c(calcRevenues(values[["sim"]], preMerger=TRUE, market = TRUE ),
                                                 calcRevenues(values[["sim"]], preMerger=FALSE, market = TRUE )
        )))


        rownames(res) <- c("Pre-Merger","Post-Merger")

        if( grepl("aids",class(values[["sim"]]),ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

        return(res)

    }, rownames = TRUE, digits=1,align="c")


    ## display results to diagnostics tab
    output$results_diagnosticsTariffs <- renderTable({

        if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

        res <- tradeDiag(valuesTariffs[["sim"]])
        res

    }, digits = 0 ,rownames = TRUE,align="c")

    output$results_diagnosticsQuota <- renderTable({

        if(input$inTabsetQuota != "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

        res <- tradeDiag(valuesQuota[["sim"]])
        res

    }, digits = 0 ,rownames = TRUE,align="c")

    ## display results to diagnostics tab
    output$results_diagnostics <- renderTable({

        if(input$inTabset!= "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

        res <- mergersDiag(values[["sim"]])
        res

    }, digits = 0 ,rownames = TRUE,align="c")


    ## display market elasticity gap to diagnostics tab
    output$results_diag_elastTariffs <- renderTable({

        if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

        res <- tradeDiag(valuesTariffs[["sim"]], mktElast=TRUE)
        res

    }, digits =2,rownames = FALSE,align="c")

    output$results_diag_elastQuota <- renderTable({

        if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

        res <- tradeDiag(valuesQuota[["sim"]], mktElast=TRUE)
        res

    }, digits =2,rownames = FALSE,align="c")

    ## display market elasticity gap to diagnostics tab
    output$results_diag_elast <- renderTable({

        if(input$inTabset!= "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

        res <- mergersDiag(values[["sim"]], mktElast=TRUE)
        res

    }, digits =2,rownames = FALSE,align="c")


    ## display parameters to diagnostics tab
    output$parametersTariffs <- renderPrint({

        if(input$inTabsetTariffs!= "diagpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

        print(getParms(valuesTariffs[["sim"]],digits=2))

    })

    output$parametersQuota <- renderPrint({

        if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

        print(getParms(valuesQuota[["sim"]],digits=2))

    })

    ## display parameters to diagnostics tab
    output$parameters <- renderPrint({

        if(input$inTabset!= "diagpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

        print(getParms(values[["sim"]],digits=2))

    })

    ## display elasticities to elasticity tab
    output$results_elastTariffs <- renderTable({

        if(input$inTabsetTariffs!= "elastpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

        isCournot <- grepl("Cournot",class(valuesTariffs[["sim"]]))

        if(input$pre_elastTariffs == "Current Tariff"){ preMerger = TRUE}
        else{preMerger =FALSE}

        if(!isCournot && input$diversionsTariffs){
            res <- diversion(valuesTariffs[["sim"]], preMerger=preMerger)
        }
        else{  res <- elast(valuesTariffs[["sim"]], preMerger=preMerger)}
        if(isCournot){colnames(res) <- "Elasticity"}

        res

    }, rownames = TRUE)

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

    ## display elasticities to elasticity tab
    output$results_elast <- renderTable({

        if(input$inTabset!= "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

        isCournot <- grepl("Cournot",class(values[["sim"]]))

        if(input$pre_elast == "Pre-Merger"){ preMerger = TRUE}
        else{preMerger =FALSE}

        if(!isCournot && input$diversions){
            res <- diversion(values[["sim"]], preMerger=preMerger)
        }
        else{  res <- elast(values[["sim"]], preMerger=preMerger)}
        if(isCournot){colnames(res) <- "Elasticity"}

        res

    }, rownames = TRUE)


    output$results_mktelastTariffs <- renderTable({

        if(input$inTabsetTariffs!= "elastpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["sim"]])){return()}

        if(input$pre_elast == "Current Tariff"){ preTariff = TRUE}
        else{preTariff =FALSE}

        res <- as.matrix(elast(valuesTariffs[["sim"]], preMerger=preTariff, market = TRUE))
        colnames(res)= "Market"
        res

    }, rownames = FALSE)

    output$results_mktelastQuota <- renderTable({

        if(input$inTabsetQuota!= "elastpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

        if(input$pre_elast == "Current Quota"){ preQuota = TRUE}
        else{preQuota =FALSE}

        res <- as.matrix(elast(valuesQuota[["sim"]], preMerger=preQuota, market = TRUE))
        colnames(res)= "Market"
        res

    }, rownames = FALSE)

    output$results_mktelast <- renderTable({

        if(input$inTabset!= "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

        if(input$pre_elast == "Pre-Merger"){ preMerger = TRUE}
        else{preMerger =FALSE}

        res <- as.matrix(elast(values[["sim"]], preMerger=preMerger, market = TRUE))
        colnames(res)= "Market"
        res

    }, rownames = FALSE)


    ## display R code to code tab
    output$results_codeTariffs <- renderPrint({

        if(input$inTabsetTariffs != "codepanelTariffs"){return()}

        thiscode <- tradeTemplateCode("Tariffs")
        cat(thiscode)
    })


    output$results_codeQuota <- renderPrint({

        if(input$inTabsetQuota != "codepanelQuota"){return()}

        thiscode <- tradeTemplateCode("Quotas")
        cat(thiscode)
    })

    ## display R code to code tab
    output$results_code <- renderPrint({

      if(input$inTabset != "codepanel"){return()}

      thiscode <- mergersTemplateCode()
      cat(thiscode)
    })


    ## display messages to message tab
    output$warningsTariffs <- renderPrint({

        if(input$inTabsetTariffs!= "msgpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["msg"]])){return()}

        print(valuesTariffs[["msg"]]$warning)
    })

    ## display messages to message tab
    output$warnings <- renderText({

        if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]]$warning)){return()}

        paste(values[["msg"]]$warning,collapse="\n")
    })

    output$warningsQuota <- renderPrint({

        if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]])){return()}

        print(valuesQuota[["msg"]]$warning)
    })

    output$errorsTariffs <- renderPrint({

        if(input$inTabsetTariffs!= "msgpanelTariffs" || input$simulateTariffs == 0 || is.null(valuesTariffs[["msg"]]$error)){cat(return())}

        print(valuesTariffs[["msg"]]$error)
    })

    output$errorsQuota <- renderPrint({

        if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]]$error)){cat(return())}

        print(valuesQuota[["msg"]]$error)
    })

    output$errors <- renderText({

        if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]]$error)){cat(return())}

        paste(values[["msg"]]$error,collapse="\n")
    })
})
