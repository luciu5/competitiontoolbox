
## Creates the graph for the Summary tab of Numerical Simulations
# Horizontal
output$plotSumATR <- renderPlot({

  if(grepl("%", input$outcomeSumATR)) ylimSumATR <- c(0,50)
  else{ylimSumATR <- c(0,350)}

  ggplot(data = subset(sumboxdata, Outcome == input$outcomeSumATR & shareOutThresh == input$shareOutSumATR), aes(x=Model, ymin=low_wisk,lower=pct25,middle=pct50,upper=pct75,ymax=high_wisk))+
    geom_boxplot(stat = "identity", lwd = 0.75, fatten = 1) +
    coord_cartesian(ylim = ylimSumATR)+
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title = element_text(size = 13), axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold")) +
    ylab(input$outcomeSumATR) +
    ggtitle(paste0(input$outcomeSumATR, ", Outside Share Less Than ", input$shareOutSumATR,"%"))

})


## Creates the graph for the Indices tab of Numerical Simulations
# Horizontal
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


## Number of Simulated Mergers for Summary Tab of ATR Numerical Simulations
# Horizontal
output$sumNumMergerATR <- renderUI({
  sumNumMerg <- subset(sumboxmktCnt, Outcome == input$outcomeSumATR & shareOutThresh == input$shareOutSumATR)
  sumNumMerg <- prettyNum(sum(sumNumMerg$Cnt), big.mark=",")
  HTML(paste("Examine the distribution of outcomes from", sumNumMerg, "simulated horizontal mergers."))
})


## Number of Simulated Mergers for Indices Tab of ATR Numerical Simulation
# Horizontal
output$indicNumMergerATR <- renderUI({
  indicNumMerg <- prettyNum(indicboxmktCnt$Cnt[which(indicboxmktCnt$Cut_type == input$indexIndATR & indicboxmktCnt$shareOutThresh == input$shareOutIndATR )], big.mark=",")
  HTML(paste("Examine the relationship between industry price changes and commmonly used merger indices from", prettyNum((indicNumMerg), big.mark=","), "simulated horizontal mergers."))
})


## Generates captions for Summary Graph of ATR Numerical Simulations
# Horizontal
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


## Generates captions for Indices Graph of ATR Numerical Simulations
# Horizontal
output$capIndATR <- renderText({
  captionIndATR()
})

captionIndATR <- reactive({
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






## Embed PNG figure for Summary tab of Vertical Numerical Simulations
# Vertical
output$figSummary <- renderImage({
  return(list(
    src = "www/surplussum.png",
    contentType = "image/png",
    width = "100%",
    height = "100%"
  ))
}, deleteFile = FALSE)

## Embed PNG figure for Upstream tab of Vertical Numerical Simulations
# Vertical
output$figUpstream <- renderImage({
  if (input$upstreamPlot == "By Bargaining Parameter") {
    return(list(
      src = "www/CVbargupBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  } else if (input$upstreamPlot == "By Number of Firms") {
    return(list(
      src = "www/CVfirmsupBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  }
}, deleteFile = FALSE)

## Embed PNG figure for Downstream tab of Vertical Numerical Simulations
# Vertical
output$figDownstream <- renderImage({
  if (input$downstreamPlot == "By Bargaining Parameter") {
    return(list(
      src = "www/CVbargdownBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  } else if (input$downstreamPlot == "By Number of Firms") {
    return(list(
      src = "www/CVfirmsdownBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  }
}, deleteFile = FALSE)

## Embed PNG figure for Vertical tab of Vertical Numerical Simulations
# Vertical
output$figVertical <- renderImage({
  if (input$verticalPlot == "By Bargaining Parameter") {
    return(list(
      src = "www/CVbargvertBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  } else if (input$verticalPlot == "By Number of Firms") {
    return(list(
      src = "www/CVfirmsvertBW.png",
      contentType = "image/png",
      width = "100%",
      height = "100%"
    ))
  }
}, deleteFile = FALSE)



## Generate captions for Summary tab of Vertical Numerical Simulations
# Vertical
output$capSummary <- renderText({
  "Caption for Summary Table"
})

## Generate captions for Upstream tab of Vertical Numerical Simulations
# Vertical
output$capUpstream <- renderText({
  captionUpstream()
})

captionUpstream <- reactive({
  switch(input$upstreamPlot, 'By Bargaining Parameter' = "by bargaining parameter, upstream",
                             'By Number of Firms' = "by number of firms, upstream")
})

## Generate captions for Downstream tab of Vertical Numerical Simulations
# Vertical
output$capDownstream <- renderText({
  captionDownstream()
})

captionDownstream <- reactive({
  switch(input$downstreamPlot, 'By Bargaining Parameter' = "by bargaining parameter, downstream",
                               'By Number of Firms' = "by number of firms, downstream")
})

## Generate captions for Vertical tab of Vertical Numerical Simulations
# Vertical
output$capVertical <- renderText({
  captionVertical()
})

captionVertical <- reactive({
  switch(input$verticalPlot, 'By Bargaining Parameter' = "by bargaining parameter, vertical",
                             'By Number of Firms' = "by number of firms, vertical")
})
