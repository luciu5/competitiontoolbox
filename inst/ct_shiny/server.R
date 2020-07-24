
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
                                       warning = w.handler), warning = W, error = E)
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

      rownames(res) <- c("Current Tariff", "New Tariff")

      if( grepl("aids",class(sim),ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

      return(res)
    }


    ##### Source Mergers and Trade functions #####
    ## Inputs
    source(paste0(getwd(), "/Inputs/mergersInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/tradeInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/reactiveInputs.R"), local = TRUE)

    ## Simulations
    source(paste0(getwd(), "/Simulations/mergersSims.R"), local = TRUE)
    source(paste0(getwd(), "/Simulations/tradeSims.R"), local = TRUE)

    ## Summary Tables
    source(paste0(getwd(), "/Summary Tables/mergersSummary.R"), local = TRUE)
    source(paste0(getwd(), "/Summary Tables/tradeSummary.R"), local = TRUE)

    ## Diagnostics Data
    source(paste0(getwd(), "/Diagnostics Data/mergersDiag.R"), local = TRUE)
    source(paste0(getwd(), "/Diagnostics Data/tradeDiag.R"), local = TRUE)

    ## Template Code
    source(paste0(getwd(), "/Template Code/mergersTemplateCode.R"), local = TRUE)
    source(paste0(getwd(), "/Template Code/tradeTemplateCode.R"), local = TRUE)


    ## Create a series of reactive values
    valuesQuota <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    valuesTariffs <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    values <- reactiveValues(inputData = mergersInputs(), sim = NULL, msg = NULL)


    ## Initialize reactive values using inputData (Charles fix?)
    observeEvent((input$menu == "Tariffs") | input$addRowsTariffs,{
      valuesTariffs[["inputData"]] <- tradeInputs(nrow = input$addRowsTariffs, type = "Tariffs")}
    )

    observeEvent((input$menu == "Quotas") | input$addRowsQuota,{
      valuesQuota[["inputData"]]<- tradeInputs(nrow = input$addRowsQuota, type = "Quotas")
    })

    observeEvent(input$menu == "Merger",{
      values[["inputData"]] <- mergersInputs()
    })


    ## Update reactive values based on user input
    observe({
      if(!is.null(input$hotTariffs)){
        valuesTariffs[["inputData"]] <- hot_to_r(input$hotTariffs)
      }

      if(!is.null(input$hotQuota)){
        valuesQuota[["inputData"]] <- hot_to_r(input$hotQuota)
      }

      if(!is.null(input$hot)){
        #values[["inputData"]] <- mergersInputs()
        values$inputData <- hot_to_r(input$hot)
        values$inputData[!is.na(values$inputData$Name) & values$inputData$Name != '', ]
      }
    })


    ## Simulate Mergers and Trade when corresponding "simulate" button is clicked by the observer
    observeEvent(input$simulate | input$simulateTariffs | input$simulateQuota , {

      if(input$menu == "Tariffs"){
        valuesTariffs[["sim"]] <- valuesTariffs[["msg"]] <-  NULL
        updateTabsetPanel(session,inputId  = "inTabsetTariffs", selected = "respanelTariffs")
        indata <- valuesTariffs[["inputData"]]

      } else if (input$menu == "Quotas"){
        valuesQuota[["sim"]] <- valuesQuota[["msg"]] <-  NULL
        updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "respanelQuota")
        indata <-   valuesQuota[["inputData"]]

      } else {
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

      ##### Run simulations based on input menu:
      if (req(input$menu) == "Tariffs") {
        indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

        ## For diagnostic purposes: output the three key inputs for the merger simulation
        print(c(supply(), demand(), as.character(elasticity())))

        thisSim <- msgCatcher(
          # Run merger simulation
          tradeSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity(),
                    type = input$menu)
        )

      } else if (input$menu == "Quotas") {
        indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

        ## For diagnostic purposes: output the three key inputs for the merger simulation
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

        ## For diagnostic purposes: output the three key inputs for the merger simulation
        print(c(supply(), demand(), as.character(elasticity())))

        thisSim <- msgCatcher(
          # Run merger simulation
          mergersSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity())

        )
        thisSim <<- thisSim  # Delete later...
      }

      thisSim$warning <- grep("are the same|INCREASE in marginal costs", thisSim$warning, value= TRUE, invert=TRUE, perl=TRUE)
      if(length(thisSim$warning) == 0){thisSim$warning = NULL}

      if (input$menu == "Tariffs") {
        valuesTariffs[["sim"]]<-  thisSim$value
        valuesTariffs[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
      } else if (input$menu =="Quotas") {
        valuesQuota[["sim"]]<-  thisSim$value
        valuesQuota[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
      } else {
        values[["sim"]] <-  thisSim$value
        values[["msg"]] <-  list(error=thisSim$error,warning=thisSim$warning)
      }

      if (!is.null(thisSim$error) || !is.null(thisSim$warning)){
        if (input$menu == "Tariffs" ) {
          updateTabsetPanel(session,inputId  = "inTabsetTariffs", selected = "msgpanelTariffs")
        } else if(input$menu == "Quotas" ) {
          updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "msgpanelQuota")
        } else if(input$menu == "Horizontal" ){
          updateTabsetPanel(session,inputId  = "inTabset", selected = "msgpanel")
        }
      }
    })


    ##### Source Output code for Mergers, Numerical Simulations, Trade objects #####
    ## Output
    source(paste0(getwd(), "/Output/mergersOutput.R"), local = TRUE)
    source(paste0(getwd(), "/Output/numericalSimsOutput.R"), local = TRUE)
    source(paste0(getwd(), "/Output/tradeOutput.R"), local = TRUE)


    ## Identify server as Internal, Local, or Public
    output$urlTextQuota <- output$urlTextTariffs <- output$urlText <- renderText({

      thisurl <- session$clientData$url_hostname

      if (grepl("atrnet\\.gov", thisurl, perl=TRUE)) {
        res <- paste(h4(span("Internal Server",style="color:blue")))
      } else if (grepl("^127", thisurl, perl=TRUE)) {
        res <- paste(h4(span("Local Server",style="color:green")))
      } else {
        res <- paste(h4(span("Public Server",style="color:red")))
      }

      res
    })


    ## Output -antitrust- and -trade- documentation
    # antitrust
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

    # trade
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













    ## Identify whether the model is over-identified in Diagnostics tab
    output$overIDTextTariffs <- renderText({

        if(is.null(valuesTariffs[["inputData"]])){return()}

        isOverID(input$supplyTariffs, input$calcElastTariffs, valuesTariffs[["InputData"]])
    })

    output$overIDTextQuota <- renderText({

        if(is.null(valuesQuota[["inputData"]])){return()}

        isOverID(input$supplyQuota, input$calcElastQuota, valuesQuota[["InputData"]])
    })

    ## Identify whether the model is over-identified in Diagnostics tab
    output$overIDText <- renderText({

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



    ## Generate no-purchase shares in Details tab
    output$results_shareOutTariffs <- renderTable({

        if(input$inTabsetTariffs!= "detpanelTariffs" || input$simulateTariffs == 0  || is.null(valuesTariffs[["sim"]])){return()}

        genShareOut(valuesTariffs[["sim"]])

    }, rownames = TRUE, digits = 1, align = "c")


    output$results_shareOutQuota <- renderTable({

        if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

        genShareOut(valuesQuota[["sim"]])

    }, rownames = TRUE, digits = 1, align = "c")

    ## Generate no-purchase shares in Details tab
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

    }, rownames = TRUE, digits = 1, align = "c")






})
