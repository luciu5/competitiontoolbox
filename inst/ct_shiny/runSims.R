
runSims <- function(supply, demand, indata, mktElast, type = c("Tariffs", "Quotas")){
  # a function to execute code from antitrust package based on ui inputs

  type <- match.arg(type)

  prices <- indata[,"Prices \n($/unit)"]
  margins <- indata$Margins

  tariffPre  <- indata[,grepl("Cur.*\\n(Tariff|Quota)",colnames(indata),perl=TRUE),drop=TRUE]
  tariffPost <- indata[,grepl("New.*\\n(Tariff|Quota)",colnames(indata),perl=TRUE),drop=TRUE]


  if(type == "Tariffs"){
    tariffPre[is.na(tariffPre)] <- 0
    tariffPost[is.na(tariffPost)] <- 0
  }
  else if(type == "Quotas"){
    ## set quota for unconstrained firms to be Inf
    tariffPre[is.na(tariffPre)] <- Inf
    tariffPost[is.na(tariffPost)] <- Inf
  }

  missPrices <- any(is.na(prices))

  shares_quantity <- shares_revenue <- indata$Output/sum(indata$Output, na.rm=TRUE)
  insideSize <- sum(indata[,"Output"], na.rm=TRUE)


  if(!missPrices){

    if(any(grepl("ces|aids", demand, perl=TRUE, ignore.case=TRUE))) insideSize <- sum(prices * indata[,"Output"], na.rm =TRUE)

    shares_revenue <- prices * shares_revenue / sum(prices * shares_revenue)
    if(supply == "2nd Score Auction") margins <- margins * prices  # convert to level margins
  }

  firstMargin <- which(!is.na(margins))[1]
  firstPrice <- which(!is.na(prices))[1]


  ownerPre = model.matrix(~-1+indata$Owner)
  ownerPre = tcrossprod(ownerPre)
  ownerPost = ownerPre


  if(supply != "Cournot" && type == "Tariffs" ){
    ownerPost =ownerPost*(1-tariffPost)
    ownerPre =ownerPre*(1-tariffPre)
  }

  ## Cournot
  # Quantities
  # quantities <- matrix(NA, nrow = length(unique(indata[[2]])), ncol = length(prices))
  # for (row in 1:nrow(indata)){
  #   quantities[as.integer(gsub("\\D", "", as.character(indata[row, 2]))), as.integer(gsub("\\D", "", as.character(indata[row, 1])))] <- indata[row, 'Output']
  # }
  #
  # # Margins
  # marginsCournot <- matrix(NA, nrow = length(unique(indata[[2]])), ncol = length(prices))
  # for (row in 1:nrow(indata)){
  #   marginsCournot[as.integer(gsub("\\D", "", as.character(indata[row, 2]))), as.integer(gsub("\\D", "", as.character(indata[row, 1])))] <- indata[row, 'Margins']
  # }


  if( type == "Tariffs"){
    switch(supply,
           Bertrand =
             switch(demand,
                    `logit (unknown elasticity)`= logit.alm(prices= prices,
                                                            shares= shares_quantity,
                                                            margins= margins,
                                                            ownerPre= ownerPre,
                                                            ownerPost= ownerPost,
                                                            insideSize = insideSize ,
                                                            mcDelta = indata$mcDelta, labels=indata$Name),
                    `aids (unknown elasticity)` = aids(prices= prices,
                                                       shares= shares_revenue,
                                                       margins= margins,
                                                       ownerPre= ownerPre,
                                                       ownerPost= ownerPost,
                                                       insideSize = insideSize ,
                                                       mcDelta = indata$mcDelta, labels=indata$Name),
                    `ces (unknown elasticity)`= ces.alm(prices= prices,
                                                        shares= shares_revenue,
                                                        margins= margins,
                                                        ownerPre= ownerPre,
                                                        ownerPost= ownerPost,
                                                        insideSize = insideSize ,
                                                        mcDelta = indata$mcDelta, labels=indata$Name),
                    linear=linear(prices= prices,
                                  quantities= indata[,"Output"],
                                  margins= margins,
                                  ownerPre= ownerPre,
                                  ownerPost= ownerPost,
                                  mcDelta = indata$mcDelta, labels=indata$Name),
                    aids=aids(prices= prices,
                              shares= shares_revenue,
                              margins= margins,
                              ownerPre= ownerPre,
                              ownerPost= ownerPost,
                              insideSize = insideSize ,
                              mcDelta = indata$mcDelta, labels=indata$Name, mktElast = mktElast),
                    logit= logit.alm(prices= prices,
                                     shares= shares_quantity,
                                     margins= margins,
                                     ownerPre= ownerPre,
                                     ownerPost= ownerPost,
                                     insideSize = insideSize ,
                                     mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast),
                    ces = ces.alm(prices= prices,
                                  shares= shares_revenue,
                                  margins= margins,
                                  ownerPre= ownerPre,
                                  ownerPost= ownerPost,
                                  insideSize = insideSize ,
                                  mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast),
                    pcaids=pcaids(prices= prices,
                                  shares= shares_revenue,
                                  knownElast = -1/margins[firstMargin],
                                  knownElastIndex = firstMargin,
                                  mktElast = mktElast,
                                  insideSize = insideSize,
                                  ownerPre= ownerPre,
                                  ownerPost= ownerPost,
                                  mcDelta = indata$mcDelta, labels=indata$Name)
             ),

           Cournot = switch(demand,
                            linear = cournot(prices = na.omit(prices)[1],
                                             #demand = rep("linear", length(prices)),
                                             demand = "linear",
                                             #cost= rep("linear", nrow(indata)),
                                             quantities = as.matrix(indata$Output),
                                             margins= as.matrix(margins),
                                             ownerPre= ownerPre,
                                             ownerPost= ownerPost,
                                             mktElast = mktElast,
                                             mcDelta = indata$mcDelta,
                                             labels=list(indata$Name, "Prod")),
                            loglinear = cournot(prices = na.omit(prices)[1],
                                                demand = "log",
                                                #cost= rep("linear", nrow(indata)),
                                                quantities = as.matrix(indata$Output),
                                                margins= as.matrix(margins),
                                                ownerPre= ownerPre,
                                                ownerPost= ownerPost,
                                                mktElast = mktElast,
                                                mcDelta = indata$mcDelta,
                                                labels=list(indata$Name, "Prod")),
                            `linear (unknown elasticity)` = cournot(prices = na.omit(prices)[1],
                                                                    demand = "linear",
                                                                    #cost= rep("linear", nrow(indata)),
                                                                    quantities = as.matrix(indata$Output),
                                                                    margins= as.matrix(margins),
                                                                    ownerPre= ownerPre,
                                                                    ownerPost= ownerPost,
                                                                    mktElast = NA_real_,
                                                                    mcDelta = indata$mcDelta,
                                                                    labels=list(indata$Name, "Prod")),
                            `loglinear (unknown elasticity)` = cournot(prices = na.omit(prices)[1],
                                                                       demand = "log",
                                                                       #cost= rep("linear", nrow(indata)),
                                                                       quantities = as.matrix(indata$Output),
                                                                       margins= as.matrix(margins),
                                                                       ownerPre= ownerPre,
                                                                       ownerPost= ownerPost,
                                                                       mktElast = NA_real_,
                                                                       mcDelta = indata$mcDelta,
                                                                       labels=list(indata$Name, "Prod"))
           )
           ,
           `2nd Score Auction`= switch(demand,
                                       `logit (unknown elasticity)` = auction2nd.logit.alm(prices= prices,
                                                                                           shares= shares_quantity,
                                                                                           margins= margins,
                                                                                           ownerPre= ownerPre,
                                                                                           ownerPost= ownerPost,
                                                                                           mcDelta = indata$mcDelta, labels=indata$Name),
                                       logit = auction2nd.logit.alm(prices= prices,
                                                                    shares= shares_quantity,
                                                                    margins= margins,
                                                                    ownerPre= ownerPre,
                                                                    ownerPost= ownerPost,
                                                                    insideSize = insideSize,
                                                                    mcDelta = indata$mcDelta, labels=indata$Name,
                                                                    mktElast = mktElast)

           )
    )
  }

  else if ( type == "Quotas"){
    switch(supply,
           Bertrand =
             switch(demand,
                    `logit (unknown elasticity)`= logit.cap.alm(prices= prices,
                                                                shares= shares_quantity,
                                                                margins= margins,
                                                                capacitiesPre = indata$Output*tariffPre,
                                                                capacitiesPost = indata$Output*tariffPost,
                                                                ownerPre= ownerPre,
                                                                ownerPost= ownerPost,
                                                                insideSize = insideSize ,
                                                                mcDelta = indata$mcDelta,
                                                                labels=indata$Name),
                    logit= logit.cap.alm(prices= prices,
                                         shares= shares_quantity,
                                         margins= margins,
                                         capacitiesPre = indata$Output*tariffPre,
                                         capacitiesPost = indata$Output*tariffPost,
                                         ownerPre= ownerPre,
                                         ownerPost= ownerPost,
                                         insideSize = insideSize ,
                                         mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast )
             )#,

           # Cournot =
           #   cournot(prices= prices[firstPrice],
           #           demand = gsub("\\s+\\(.*","",demand,perl=TRUE),
           #           cost= rep("linear", nrow(indata)),
           #           quantities = as.matrix(indata[,"Output"]),
           #           margins= as.matrix(margins),
           #           ownerPre= ownerPre,
           #           ownerPost= ownerPost,
           #           mktElast = ifelse( grepl("unknown elasticity", demand),
           #                              NA_real_, mktElast),
           #           mcDelta = indata$mcDelta,
           #           labels=list(as.character(indata$Name),indata$Name[firstPrice]))
    )

  }
}
