
tradeSims <- function(supply, demand, indata, mktElast, type = c("Tariffs", "Quotas")){
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

  }

  # firstMargin <- which(!is.na(margins))[1]
  # firstPrice <- which(!is.na(prices))[1]
  #
  #
  # ownerPre = model.matrix(~-1+indata$Owner)
  # ownerPre = tcrossprod(ownerPre)
  # ownerPost = ownerPre
  #
  #
  # if(supply == "Bertrand" && type == "Tariffs" ){
  #   ownerPost =ownerPost*(1-tariffPost)
  #   ownerPre =ownerPre*(1-tariffPre)
  # }

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

  indata <<- indata
  if(type == "Tariffs"){
    switch(supply,
           `Monopolistic Competition` =
             switch(demand,
                    `logit (unknown elasticity)`      =          monopolistic_competition_tariff(
                                                                         demand="logit",
                                                                         prices=prices,
                                                                         quantities = indata$Output,
                                                                         margins= margins,
                                                                         tariffPre=tariffPre,
                                                                         tariffPost=tariffPost,
                                                                         labels=indata$Name
                                                                        ),
                    `ces (unknown elasticity)`      =          monopolistic_competition_tariff(
                                                                                                demand="ces",
                                                                                                prices=prices,
                                                                                                quantities = indata$Output,
                                                                                                margins= margins,
                                                                                                tariffPre=tariffPre,
                                                                                                tariffPost=tariffPost,
                                                                                                labels=indata$Name
                                                                                              ),
                    `logit`                           =          monopolistic_competition_tariff(
                                                                          demand=demand,
                                                                          prices=prices,
                                                                          quantities = indata$Output,
                                                                          margins= margins,
                                                                          tariffPre=tariffPre,
                                                                          tariffPost=tariffPost,
                                                                          labels=indata$Name,mktElast = mktElast
                                                                        ),
                    `ces`                           =          monopolistic_competition_tariff(
                                                                          demand=demand,
                                                                          prices=prices,
                                                                          quantities = indata$Output,
                                                                          margins= margins,
                                                                          tariffPre=tariffPre,
                                                                          tariffPost=tariffPost,
                                                                          labels=indata$Name,mktElast = mktElast
                                                                        )
             ),
           Bertrand =
             switch(demand,
                    logit = bertrand_tariff(demand = demand,
                                            prices= prices,
                                            quantities = indata$Output,
                                            margins= margins,
                                            owner = indata$Owner,
                                            tariffPre = tariffPre,
                                            tariffPost = tariffPost,
                                            labels = indata$Name,
                                            mktElast = mktElast),
                    ces = bertrand_tariff(demand = demand,
                                          prices= prices,
                                          quantities = indata$Output,
                                          margins= margins,
                                          owner = indata$Owner,
                                          tariffPre = tariffPre,
                                          tariffPost = tariffPost,
                                          labels = indata$Name,
                                          mktElast = mktElast),
                    aids = bertrand_tariff(demand = demand,
                                           prices= prices,
                                           quantities = indata$Output,
                                           margins= margins,
                                           owner = indata$Owner,
                                           tariffPre = tariffPre,
                                           tariffPost = tariffPost,
                                           labels = indata$Name,
                                           mktElast = mktElast),
                    `logit (unknown elasticity)`= bertrand_tariff(demand = "logit",
                                                                  prices= prices,
                                                                  quantities = indata$Output,
                                                                  margins= margins,
                                                                  owner = indata$Owner,
                                                                  tariffPre = tariffPre,
                                                                  tariffPost = tariffPost,
                                                                  labels = indata$Name),
                    `ces (unknown elasticity)`= bertrand_tariff(demand = "ces",
                                                                prices= prices,
                                                                quantities = indata$Output,
                                                                margins= margins,
                                                                owner = indata$Owner,
                                                                tariffPre = tariffPre,
                                                                tariffPost = tariffPost,
                                                                labels = indata$Name),
                    `aids (unknown elasticity)` = bertrand_tariff(demand = "aids",
                                                                  prices= prices,
                                                                  quantities = indata$Output,
                                                                  margins= margins,
                                                                  owner = indata$Owner,
                                                                  tariffPre = tariffPre,
                                                                  tariffPost = tariffPost,
                                                                  labels = indata$Name)
             ),

           Cournot = switch(demand,
                            linear = cournot_tariff(prices = na.omit(prices)[1],
                                                   #demand = rep("linear", length(prices)),
                                                   demand = demand,
                                                   #cost= rep("linear", nrow(indata)),
                                                   quantities = as.matrix(indata$Output),
                                                   margins= as.matrix(margins),
                                                   owner = indata$Owner,
                                                   tariffPre = as.matrix(tariffPre),
                                                   tariffPost = as.matrix(tariffPost),
                                                   mktElast = mktElast,
                                                   labels = list(indata$Name, "Prod")),
                            loglinear = cournot_tariff(prices = na.omit(prices)[1],
                                                demand = "log",
                                                #cost= rep("linear", nrow(indata)),
                                                quantities = as.matrix(indata$Output),
                                                margins= as.matrix(margins),
                                                ownerPre= ownerPre,
                                                ownerPost= ownerPost,
                                                mktElast = mktElast,
                                                mcDelta = indata$mcDelta,
                                                labels=list(indata$Name, "Prod")),
                            `linear (unknown elasticity)` = cournot_tariff(prices = na.omit(prices)[1],
                                                                    demand = "linear",
                                                                    #cost= rep("linear", nrow(indata)),
                                                                    quantities = as.matrix(indata$Output),
                                                                    margins= as.matrix(margins),
                                                                    ownerPre= ownerPre,
                                                                    ownerPost= ownerPost,
                                                                    mktElast = NA_real_,
                                                                    mcDelta = indata$mcDelta,
                                                                    labels=list(indata$Name, "Prod")),
                            `loglinear (unknown elasticity)` = cournot_tariff(prices = na.omit(prices)[1],
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

    )
  }

  else if ( type == "Quotas"){
    switch(supply,
           Bertrand =
             switch(demand,
                    `logit (unknown elasticity)`= bertrand_quota(prices= prices,
                                                                shares= shares_quantity,
                                                                margins= margins,
                                                                capacitiesPre = indata$Output*tariffPre,
                                                                capacitiesPost = indata$Output*tariffPost,
                                                                ownerPre= ownerPre,
                                                                ownerPost= ownerPost,
                                                                insideSize = insideSize ,
                                                                mcDelta = indata$mcDelta,
                                                                labels=indata$Name),
                    logit = bertrand_quota(prices= prices,
                                         shares= shares_quantity,
                                         margins= margins,
                                         capacitiesPre = indata$Output*tariffPre,
                                         capacitiesPost = indata$Output*tariffPost,
                                         ownerPre= ownerPre,
                                         ownerPost= ownerPost,
                                         insideSize = insideSize ,
                                         mcDelta = indata$mcDelta, labels=indata$Name, mktElast = mktElast)
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
