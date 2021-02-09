
tradeTemplateCode <- function(type){

  if (type == "Tariffs") {
    indata <- valuesTariffs[["inputData"]]
  } else if (type == "Quotas") {
    indata <- valuesQuota[["inputData"]]
  }

  indata <- indata[!is.na(indata$Name) & indata$Name != '',]

  cnames <- colnames(indata)
  cnames <- gsub("\n","",cnames)

  firstPrice <- which(!is.na(indata[,grep("price",cnames, ignore.case = TRUE)]))[1]

  argnames <- c("demand",
                "owner",
                "prices",
                "quantities",
                "margins",
                "tariffPre",
                "tariffPost"
  )

  argvalues   <- paste0(argnames," = simdata$`",cnames,"`")
  summary_code <- "summary(simres, levels = FALSE, market=FALSE)"

  if(!is.null(demand()) & !is.null(supply())){

    if (type == "Tariffs"){
      thisElast <- ifelse(grepl("elast",input$calcElastTariffs),
                          input$enterElastTariffs,
                          "NA_real_"
      )

      if(grepl("logit", demand(), ignore.case = TRUE)){
        thisSize <- "sum(simdata$`Quantities`)"
      }
      else if(all(is.na(indata[,grepl("price",cnames, ignore.case = TRUE)]))){

        thisSize <-   "sum(simdata$`Revenues`)"
      }

      else{

        thisSize <- paste0("sum(simdata$`",grep("price",cnames, ignore.case = TRUE, value=TRUE),"`*simdata$`Quantities`)")
      }

      thisdemand <- gsub("\\s*\\(.*","", demand() ,perl=TRUE)

      argvalues[1] <- paste(c("demand = ", shQuote(thisdemand)), collapse = "")

      argvalues <- c(argvalues,
                     paste0("mktElast = ", thisElast,collapse = ""),
                     #paste0("insideSize = ",thisSize, collapse=""),
                     "labels = simdata$Name"
      )

      if(input$supplyTariffs == "Cournot"){
        atrfun <- "cournot_tariff"
        argvalues[grep("prices", argvalues)] <- paste0(argvalues[grep("prices", argvalues)],"[",firstPrice,"]")
        argvalues[grep("quantities", argvalues)] <- "quantities = as.matrix(simdata$`Quantities`)"
        argvalues[grep("margins", argvalues)] <- paste0("margins = as.matrix(simdata$`",grep("Margin",cnames,value = TRUE),"`)")
        argvalues[grep("tariffPre", argvalues)] <- paste0("tariffPre = as.matrix(simdata$`",grep("Current Tariff",cnames,value = TRUE),"`)")
        argvalues[grep("tariffPost", argvalues)] <- paste0("tariffPost = as.matrix(simdata$`",grep("New Tariff",cnames,value = TRUE),"`)")
        argvalues[grep("labels", argvalues)] <- sprintf("labels = list(as.character(simdata$Name),as.character(simdata$Name[%d]))",firstPrice)
        argvalues <- argvalues[grep("insideSize", argvalues, invert = TRUE)]
      }
      else if( input$supplyTariffs =="Bertrand"){atrfun <- "bertrand_tariff"}
      else if( input$supplyTariffs =="Monopolistic Competition"){
        atrfun <- "monopolistic_competition_tariff"

        argvalues <- argvalues[grep("owner", argvalues, invert = TRUE)]

        summary_code <- "summary(simres, levels = FALSE, market=FALSE)"
        }
      else{atrfun <- "auction2nd.logit.alm"
      argvalues <- argvalues[-1]
      argvalues[grep("quantities", argvalues)] <- "shares = simdata$`Quantities` / sum( simdata$`Quantities` ) "
      argvalues[grep("margins", argvalues)] <- paste0("margins = simdata$`",
                                                      grep("Margin",cnames,value = TRUE),
                                                      "` * ", "simdata$`", grep("Price", cnames, value=TRUE),"`")
      }
    } else {

      thisElast <- ifelse(grepl("elast",input$calcElastQuota),
                          input$enterElastQuota,
                          "NA_real_"
      )

      if(req(grepl("logit", demand(), ignore.case =TRUE))){
        thisSize <- "sum(simdata$`Quantities`)"
      }
      else if(all(is.na(indata[,grepl("price",cnames, ignore.case = TRUE)]))){
        thisSize <- "sum(simdata$`Revenues`)"
      }

      else{
        thisSize <- paste0("sum(simdata$`",grep("price",cnames, ignore.case = TRUE, value=TRUE),"`*simdata$`Quantities`)")
      }

      thisdemand <- gsub("\\s*\\(.*","", demand() ,perl=TRUE)

      argvalues[1] <- paste(c("demand = ", shQuote(thisdemand)), collapse = "")

      argvalues <- c(argvalues,
                     paste0("mktElast = ", thisElast,collapse = ""),
                     #paste0("insideSize = ",thisSize, collapse=""),
                     "labels = simdata$Name"
      )

      if(input$supplyQuota == "Cournot"){
        atrfun <- "cournot_tariff"
        argvalues[grep("prices", argvalues)] <- paste0(argvalues[grep("prices", argvalues)],"[",firstPrice,"]")
        argvalues[grep("quantities", argvalues)] <- "quantities = as.matrix(simdata$`Quantities`)"
        argvalues[grep("margins", argvalues)] <- paste0("margins = as.matrix(simdata$`",grep("Margin",cnames,value = TRUE),"`)")
        argvalues[grep("tariffPre", argvalues)] <- paste0("tariffPre = as.matrix(simdata$`",grep("Current Tariff",cnames,value = TRUE),"`)")
        argvalues[grep("tariffPost", argvalues)] <- paste0("tariffPost = as.matrix(simdata$`",grep("New Tariff",cnames,value = TRUE),"`)")
        argvalues[grep("labels", argvalues)] <- sprintf("labels = list(as.character(simdata$Name),as.character(simdata$Name[%d]))",firstPrice)
        argvalues <- argvalues[grep("insideSize", argvalues, invert = TRUE)]
      }
      else if( input$supplyQuota =="Bertrand"){atrfun <- "bertrand_tariff"}
      else{atrfun <- "auction2nd.logit.alm"
      argvalues <- argvalues[-1]
      argvalues[grep("quantities", argvalues)] <- "shares = simdata$`Quantities` / sum( simdata$`Quantities` ) "
      argvalues[grep("margins", argvalues)] <- paste0("margins = simdata$`",
                                                      grep("Margin",cnames,value = TRUE),
                                                      "` * ", "simdata$`", grep("Price", cnames, value=TRUE),"`")
      }
    }

    atrfun <- paste0("simres <- ",atrfun,"(\n\t",paste0(argvalues,collapse = ",\n\t"),")",collapse = "\n")

    indata_code <- sapply(1:ncol(indata),
                          function(x){d <- indata[,x];
                          if(is.character(d)){d <- sprintf("'%s'", indata[,x])};
                          paste0('`',cnames[x],'`',"= c(",paste(d,collapse=","),")")}
    )
    indata_code <- paste0("simdata <- data.frame(\n\t", paste0(indata_code,collapse=",\n\t"),",\n check.names = FALSE,\n stringsAsFactors = FALSE\n)")

    if(type =="Quotas"){
      atrfun <- gsub("tariff","quota",atrfun)
      indata_code <- gsub("Tariff","Quota",indata_code)
    }

    thiscode <- c(
      "library(trade)",
      "\n\n ## Load Data:\n",
      indata_code,
      "\n\n ## Run Simulation: \n",
      atrfun,
      "\n\n ## Summary Tab Results:\n",
      gsub("market\\s*=\\s*FALSE","market=TRUE",summary_code,perl=TRUE),
      "\n\n ## Details Tab Results:\n",
      summary_code,
      "\n\n",
      "\n ## Elasticities Tab Results  (Pre-tariff Only):\n",
      "elast(simres, preMerger = TRUE, market=TRUE)\n elast(simres, preMerger = TRUE, market=FALSE)",
      "\n\n ## Diagnostics Tab Results:\n",
      "calcDiagnostics(simres)\n\n"
    )

    return(thiscode)
  }
}
