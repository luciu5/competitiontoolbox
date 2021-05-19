
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


## Load competitiontoolbox datasets
# Simulated outcome data
data("sumboxdata", package = "competitiontoolbox")
data("sumboxdata_trade", package = "competitiontoolbox")
data("indicboxdata", package = "competitiontoolbox")
# Simulation count data
data("sumboxmktCnt", package = "competitiontoolbox")
data("sumboxmktCnt_trade", package = "competitiontoolbox")
data("indicboxmktCnt", package = "competitiontoolbox")


## Run server-side Shiny program
shinyServer(function(input, output, session) {

    nPossProds <- 10  # Only allow 10 products by default

    msgCatcher <- function(expr){

      W <- NULL
      E <- NULL

      w.handler <- function(w){  # warning handler
          W <<- append(W,conditionMessage(w))
          invokeRestart("muffleWarning")
      }

      e.handler <- function(e){  # error handler
          E <<- append(E, conditionMessage(e))
          NULL
      }

      list(value = withCallingHandlers(tryCatch(expr, error = e.handler),
                                       warning = w.handler), warning = W, error = E)
    }


    ## Update calcElastTariffs radio button
    observe({
      if (input$supplyTariffs == "Bertrand") {
        updateRadioButtons(
          session,
          inputId = "calcElastTariffs",
          choices = c("market elasticity AND 1 or more margins",
                      "2 or more margins")
        )
      } else {
        updateRadioButtons(
          session,
          inputId = "calcElastTariffs",
          choices = c("market elasticity AND 1 or more margins",
                      "1 or more margins")
        )
      }
    })


    ##### Source Mergers and Trade functions #####
    ## Inputs
    source(paste0(getwd(), "/Inputs/mergersInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/tradeInputs.R"), local = TRUE)
    source(paste0(getwd(), "/Inputs/reactiveInputs.R"), local = TRUE)

    ## Simulations
    source(paste0(getwd(), "/Simulations/mergersSims.R"), local = TRUE)
    source(paste0(getwd(), "/Simulations/tradeSims.R"), local = TRUE)

    ## Summary Tables
    source(paste0(getwd(), "/Summary/mergersSummary.R"), local = TRUE)
    source(paste0(getwd(), "/Summary/tradeSummary.R"), local = TRUE)

    ## No-purchase Shares
    source(paste0(getwd(), "/Details/mergersNoPurch.R"), local = TRUE)
    source(paste0(getwd(), "/Details/tradeNoPurch.R"), local = TRUE)

    ## Diagnostics Data
    source(paste0(getwd(), "/Diagnostics/mergersDiag.R"), local = TRUE)
    source(paste0(getwd(), "/Diagnostics/tradeDiag.R"), local = TRUE)
    ## Identification
    source(paste0(getwd(), "/Diagnostics/isOverID.R"), local = TRUE)

    ## Template Code
    source(paste0(getwd(), "/R/mergersTemplateCode.R"), local = TRUE)
    source(paste0(getwd(), "/R/tradeTemplateCode.R"), local = TRUE)


    ## Create a series of reactive values
    valuesQuota <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    valuesTariffs <- reactiveValues(inputData = tradeInputs(nPossProds), sim = NULL, msg = NULL)
    valuesVertical <- reactiveValues(inputData = mergersInputs(), sim = NULL, msg = NULL)
    values <- reactiveValues(inputData = mergersInputs(), sim = NULL, msg = NULL)


    ## Initialize reactive values using inputData (Charles fix?)
    observeEvent((input$menu == "Tariffs") | input$addRowsTariffs, {
      valuesTariffs[["inputData"]] <- tradeInputs(nrow = input$addRowsTariffs, type = "Tariffs")
    })

    observeEvent((input$menu == "Quotas") | input$addRowsQuota, {
      valuesQuota[["inputData"]] <- tradeInputs(nrow = input$addRowsQuota, type = "Quotas")
    })

    observeEvent(input$menu == "Vertical", {
      valuesVertical[["inputData"]] <- mergersInputs(nrow = input$addRowsVertical, type = "Vertical")
    })

    observeEvent(input$menu == "Horizontal", {
      values[["inputData"]] <- mergersInputs(type = "Horizontal")
    })


    ## Update reactive values based on user input
    observe({
      if(!is.null(input$hotTariffs)){
        valuesTariffs[["inputData"]] <- hot_to_r(input$hotTariffs)
      }

      if(!is.null(input$hotQuota)){
        valuesQuota[["inputData"]] <- hot_to_r(input$hotQuota)
      }

      if(!is.null(input$hotVertical)){
        #valuesVertical[["inputData"]] <- mergersInputs(type = "Vertical")
        valuesVertical$inputData <- hot_to_r(input$hotVertical)
        valuesVertical$inputData[!is.na(valuesVertical$inputData$Name) & valuesVertical$inputData$Name != '', ]
      }

      if(!is.null(input$hot)){
        #values[["inputData"]] <- mergersInputs()
        values$inputData <- hot_to_r(input$hot)
        values$inputData[!is.na(values$inputData$Name) & values$inputData$Name != '', ]
      }
    })


    ## Simulate Mergers and Trade when corresponding "simulate" button is clicked by the observer
    observeEvent(input$simulate | input$simulateVertical | input$simulateTariffs | input$simulateQuota, {

      if(input$menu == "Tariffs"){
        valuesTariffs[["sim"]] <- valuesTariffs[["msg"]] <-  NULL
        updateTabsetPanel(session, inputId  = "inTabsetTariffs", selected = "respanelTariffs")
        indata <- valuesTariffs[["inputData"]]

      } else if (input$menu == "Quotas"){
        valuesQuota[["sim"]] <- valuesQuota[["msg"]] <-  NULL
        updateTabsetPanel(session, inputId  = "inTabsetQuota", selected = "respanelQuota")
        indata <-   valuesQuota[["inputData"]]

      } else if (input$menu == "Vertical") {
        valuesVertical[["sim"]] <- valuesVertical[["msg"]] <-  NULL
        updateTabsetPanel(session, inputId  = "inTabsetVertical", selected = "respanelVertical")
        indata <- valuesVertical[["inputData"]]

      } else {
        values[["sim"]] <- values[["msg"]] <-  NULL
        updateTabsetPanel(session, inputId  = "inTabset", selected = "respanel")
        indata <- values[["inputData"]]
      }

      # Standardize "Margin" and "Output" column names of horizontal merger sims
      # Drop products from horizontal merger sim inputs that have missing market shares
      if (input$menu != "Vertical") {
        colnames(indata)[grepl("Margins", colnames(indata), perl = TRUE)] <- "Margins"
        colnames(indata)[grepl("Quantities|Revenues", colnames(indata), perl = TRUE)] <- "Output"

        indata <- indata[!is.na(indata[, "Output"]), ]
        indata$mcDelta <- 0
      }

      # Drop products from vertical merger sim inputs that have missing downstream market shares
      if (input$menu == "Vertical") {
        indata <- indata[!is.na(indata[, "sharesDown"]), ]
      }

      if (input$menu == "Tariffs") {
        tariffPost <- indata[,grep('New.*\\n(Tariff)',colnames(indata), perl=TRUE), drop = TRUE]
        tariffPost[is.na(tariffPost)] <- 0

        tariffPre <- indata[,grep('Cur.*\\n(Tariff)',colnames(indata), perl = TRUE), drop= TRUE]
        tariffPre[is.na(tariffPre)] <- 0
        indata$mcDelta <- (tariffPost - tariffPre)
        indata$mcDelta <- indata$mcDelta/(1 - tariffPost)
      }


      ##### Run simulations based on input$menu:
      if (req(input$menu) == "Tariffs") {
        indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

        ## For diagnostic purposes: output the three key inputs for the merger simulation
        print(c(supply(), demand(), as.character(elasticity())))

        thisSim <- msgCatcher(
          # Run merger simulation
          tradeSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity(),
                    type = input$menu)
        )

        thisSim <<- thisSim  # Delete later

      } else if (input$menu == "Quotas") {
        indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner))

        ## For diagnostic purposes: output the three key inputs for the merger simulation
        print(c(supply(), demand(), as.character(elasticity())))

        thisSim <- msgCatcher(
          # Run merger simulation
          tradeSims(supply = supply(),demand = demand(), indata = indata, mktElast = elasticity(),
                    type = input$menu)
        )

        thisSim <<- thisSim  # Delete later

      } else if (input$menu == "Vertical") {

        ## For diagnostic purposes: output the two key inputs for the vertical merger simulation
        print(c(supply(), demand()))

        thisSim <- msgCatcher(
          # Run merger simulation
          mergersSims(supply = supply(), indata = indata,
                      type = "Vertical")
        )

        thisSim <<- thisSim  # Delete later

      } else {

        indata$mcDelta <- indata[, grep('Cost Changes', colnames(indata))]
        indata$mcDelta[is.na(indata$mcDelta)] <- 0

        indata$'Pre-merger\n Owner' <- factor(indata$'Pre-merger\n Owner',levels=unique(indata$'Pre-merger\n Owner'))
        indata$'Post-merger\n Owner' <- factor(indata$'Post-merger\n Owner',levels=unique(indata$'Post-merger\n Owner'))

        ## For diagnostic purposes: output the three key inputs for the merger simulation
        print(c(supply(), demand(), as.character(elasticity())))

        thisSim <- msgCatcher(
          # Run merger simulation
          mergersSims(supply = supply(), demand = demand(), indata = indata, mktElast = elasticity(),
                      type = "Horizontal")
        )

        thisSim <<- thisSim  # Delete later
      }

      thisSim$warning <- grep("are the same|INCREASE in marginal costs", thisSim$warning, value = TRUE, invert = TRUE, perl = TRUE)
      if(length(thisSim$warning) == 0){thisSim$warning = NULL}

      if (input$menu == "Tariffs") {
        valuesTariffs[["sim"]] <- thisSim$value
        valuesTariffs[["msg"]] <- list(error = thisSim$error, warning = thisSim$warning)
      } else if (input$menu =="Quotas") {
        valuesQuota[["sim"]] <- thisSim$value
        valuesQuota[["msg"]] <- list(error = thisSim$error, warning = thisSim$warning)
      } else if (input$menu == "Vertical") {
        valuesVertical[["sim"]] <- thisSim$value
        valuesVertical[["msg"]] <- list(error = thisSim$error, warning = thisSim$warning)
      } else {
        values[["sim"]] <- thisSim$value
        values[["msg"]] <- list(error = thisSim$error, warning = thisSim$warning)
      }

      if (!is.null(thisSim$error) || !is.null(thisSim$warning)){
        if (input$menu == "Tariffs") {
          updateTabsetPanel(session, inputId = "inTabsetTariffs", selected = "msgpanelTariffs")
        } else if(input$menu == "Quotas") {
          updateTabsetPanel(session, inputId = "inTabsetQuota", selected = "msgpanelQuota")
        } else if(input$menu == "Vertical") {
          updateTabsetPanel(session, inputId = "inTabsetVertical", selected = "msgpanelVertical")
        } else if(input$menu == "Horizontal") {
          updateTabsetPanel(session, inputId = "inTabset", selected = "msgpanel")
        }
      }
    })


    ##### Source Output code for Mergers, Numerical Simulations, Trade objects #####
    ## Output
    source(paste0(getwd(), "/Output/mergersOutput.R"), local = TRUE)
    source(paste0(getwd(), "/Output/numericalSimsOutput.R"), local = TRUE)
    source(paste0(getwd(), "/Output/tradeOutput.R"), local = TRUE)


    ## Identify server as Internal, Local, or Public
    output$urlTextQuota <- output$urlTextTariffs <- output$urlTextVertical <- output$urlText <- renderText({

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

})
