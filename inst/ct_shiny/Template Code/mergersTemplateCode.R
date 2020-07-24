
mergersTemplateCode <- function(){

  indata <- values[["inputData"]]
  indata <- indata[!is.na(indata$Name) & indata$Name != '',]
  cat(NULL)

  cnames <- colnames(indata)
  cnames <- gsub("\n","",cnames)

  firstPrice <- which(!is.na(indata[,grep("price",cnames, ignore.case = TRUE)]))[1]

  argnames <- c("demand",
                "ownerPre",
                "ownerPost",
                "prices",
                "quantities",
                "margins",
                "mcDelta"

  )

  argvalues   <- paste0(argnames," = simdata$`",cnames,"`")

  thisElast <- ifelse(grepl("elast",input$calcElast),
                      input$enterElast,
                      "NA_real_"
  )

  if(!is.null(demand()) & !is.null(supply())){

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
                   paste0("insideSize = ", thisSize, collapse=""),
                   "labels = simdata$Name"
    )


    if(supply() == "Cournot"){
      atrfun <- "cournot"
      argvalues[grep("prices", argvalues)] <- paste0(argvalues[grep("prices", argvalues)],"[",firstPrice,"]")
      argvalues[grep("quantities", argvalues)] <- "quantities = as.matrix(simdata$`Quantities`)"
      argvalues[grep("margins", argvalues)] <- paste0("margins = as.matrix(simdata$`",grep("Margin",cnames,value = TRUE),"`)")

      argvalues[grep("labels", argvalues)] <- sprintf("labels = list(as.character(simdata$Name),as.character(simdata$Name[%d]))",firstPrice)
      #argvalues[grep("insideSize", argvalues)] <- NULL
    }
    else if(supply() =="Bertrand"){atrfun <- "bertrand.alm"}

    else{atrfun <- "auction2nd.logit.alm"
    argvalues <- argvalues[-1]
    argvalues[grep("quantities", argvalues)] <- "shares = simdata$`Quantities` / sum( simdata$`Quantities` ) "
    argvalues[grep("margins", argvalues)] <- paste0("margins = simdata$`",
                                                    grep("Margin",cnames,value = TRUE),
                                                    "` * ", "simdata$`", grep("Price", cnames, value=TRUE),"`")
    }

    atrfun <- paste0("simres <- ",atrfun,"(\n\t",paste0(argvalues,collapse = ",\n\t"),")",collapse = "\n")

    indata_code <- sapply(1:ncol(indata),
                          function(x){d <- indata[,x];
                          if(is.character(d)){d <- sprintf("'%s'", indata[,x])};
                          paste0('`',cnames[x],'`',"= c(",paste(d,collapse=","),")")}
    )
    indata_code <- paste0("simdata <- data.frame(\n\t", paste0(indata_code,collapse=",\n\t"),",\n check.names = FALSE,\n stringsAsFactors = FALSE\n)")

    thiscode <- c(
      "library(antitrust)",
      "\n\n ## Load Data:\n",
      indata_code,
      "\n\n ## Run Simulation: \n",
      atrfun,
      "\n\n ## Summary Tab Results:\n",
      "summary(simres, revenues = FALSE, levels = FALSE, market=TRUE)",
      "\n\n ## Details Tab Results:\n",
      "summary(simres, revenues = FALSE, levels = FALSE, market=FALSE)\n\n",
      "\n\n ## Elasticities Tab Results  (Pre-merger Only):\n",
      "elast(simres, preMerger = TRUE, market=TRUE)\n elast(simres, preMerger = TRUE, market=FALSE)",
      "\n\n ## Diagnostics Tab Results:\n",
      "calcDiagnostics(simres)\n\n"
    )

    return(thiscode)
  }
}
