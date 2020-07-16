
runSimsMergers <- function(supply, demand, indata, mktElast){
  # a function to execute code from antitrust package based on ui inputs

  prices <- indata[,"Prices \n($/unit)"]
  margins <- indata$Margins

  missPrices <- any(is.na(prices))

  shares_quantity <- shares_revenue <- indata$Output/sum(indata$Output, na.rm=TRUE)
  insideSize <- sum(indata[,"Output"], na.rm=TRUE)


  if(!missPrices){

    if(any(grepl("ces|aids",demand,perl=TRUE,ignore.case=TRUE))) insideSize <- sum(prices * indata[,"Output"], na.rm =TRUE)

    shares_revenue <- prices * shares_revenue / sum(prices * shares_revenue)
    if(supply == "2nd Score Auction") margins <- margins * prices # convert to level margins
  }

  firstMargin <- which(!is.na(margins))[1]
  firstPrice <- which(!is.na(prices))[1]


  ownerPre = model.matrix(~-1+indata$'Pre-merger\n Owner')
  ownerPre = tcrossprod(ownerPre)
  if(nlevels(indata$'Post-merger\n Owner') > 1){
    ownerPost = model.matrix(~-1+indata$'Post-merger\n Owner')
    ownerPost = tcrossprod(ownerPost)
  }
  else{ownerPost <- matrix(1, ncol=length(indata$Output), nrow = length(indata$Output))}

  ## Cournot
  # Quantities
  indata <<- indata
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


  switch(supply,
         Bertrand =
           switch(demand,
                  `logit (unknown elasticity)` = logit.alm(prices= prices,
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
                                   mcDelta = indata$mcDelta, labels=indata$Name, mktElast = mktElast),
                  ces = ces.alm(prices= prices,
                                shares= shares_revenue,
                                margins= margins,
                                ownerPre= ownerPre,
                                ownerPost= ownerPost,
                                insideSize = insideSize ,
                                mcDelta = indata$mcDelta, labels=indata$Name, mktElast = mktElast),
                  pcaids = pcaids(prices= prices,
                                  shares= shares_revenue,
                                  knownElast = -1/margins[firstMargin],
                                  knownElastIndex = firstMargin,
                                  mktElast = mktElast,
                                  insideSize = insideSize,
                                  ownerPre= ownerPre,
                                  ownerPost= ownerPost,
                                  mcDelta = indata$mcDelta,
                                  labels=indata$Name)
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
         ),

         `2nd Score Auction`= switch(demand,
                                     `logit (unknown elasticity)` = auction2nd.logit.alm(prices= prices,
                                                                                         shares= shares_quantity,
                                                                                         margins= margins,
                                                                                         ownerPre= ownerPre,
                                                                                         ownerPost= ownerPost,
                                                                                         insideSize = insideSize,
                                                                                         mcDelta = indata$mcDelta,
                                                                                         labels=indata$Name),
                                     logit = auction2nd.logit.alm(prices= prices,
                                                                  shares= shares_quantity,
                                                                  margins= margins,
                                                                  ownerPre= ownerPre,
                                                                  ownerPost= ownerPost,
                                                                  insideSize = insideSize,
                                                                  mcDelta = indata$mcDelta,
                                                                  labels=indata$Name,
                                                                  mktElast = mktElast)

         )
  )
}
