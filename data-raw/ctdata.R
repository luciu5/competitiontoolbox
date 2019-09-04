## code to prepare `bwplotdata` dataset goes here

rm(list=ls())

library(devtools, quietly=TRUE)
library(dplyr,quietly = TRUE)
library(tidyr,quietly = TRUE)
library(ggplot2,quietly = TRUE)



load("./data-raw/results.RData")


indicdata <- ungroup(res_flat)%>% filter(isMarket & isHSR) %>% mutate_if(is.factor,factor) %>%
  select(supply,demand,shareOut,`Industry Price Change (%)` ,nFirms,#partyShare,upp,cmcr,harm_2nd,
         hhicut,hhideltacut,uppcut,cmcrcut,harm_2ndcut,`Party Gap`)

indicdata <- gather(indicdata, key=Cut_type,value=Cut_value,contains("cut"),nFirms,`Party Gap`) %>%
  mutate( Cut_type=ifelse(Cut_type == "hhicut","HHI",Cut_type),
          Cut_type=ifelse(Cut_type == "hhideltacut","Delta HHI",Cut_type),
          Cut_type=ifelse(Cut_type == "uppcut","UPP",Cut_type),
          Cut_type=ifelse(Cut_type == "cmcrcut","CMCR",Cut_type),
          Cut_type=ifelse(Cut_type == "harm_2ndcut","Harm2nd",Cut_type),
          Cut_type=ifelse(Cut_type == "nFirms","Firm Count",Cut_type)
  ) %>% filter(!is.na(Cut_value)) %>%
  rename( Supply=supply,Demand=demand)



calcBoxStatsInd <- function(thresh=0.1, pooled=TRUE,reportN=FALSE){

  if(reportN){

    return(
      filter(indicdata ,shareOut >= thresh & !is.na(Cut_value) ) %>% select(Cut_type) %>%
        group_by(Cut_type) %>% summarise(Cnt=n()) %>%
        mutate(
          shareOutThresh = as.integer(thresh*100))
    )
  }


  if(pooled){


    filter(indicdata ,shareOut >= thresh & !is.na(Cut_value) ) %>% select(Cut_type,Cut_value,`Industry Price Change (%)`) %>%
      group_by(Cut_type,Cut_value) %>%
      do(data.frame(bp_stat=c("low_wisk","pct25","pct50","pct75","high_wisk"),
                    bp_value=boxplot.stats(.$`Industry Price Change (%)` )$stats)) %>%
      mutate(
        shareOutThresh = as.integer(thresh*100),Supply="Pooled",Demand="Pooled") %>% spread(key=bp_stat,value=bp_value)

  }

  else{

    filter(indicdata ,shareOut < thresh & !is.na(Cut_value) ) %>% select(Supply,Demand,Cut_type,Cut_value,`Industry Price Change (%)`) %>%
      group_by(Supply,Demand,Cut_type,Cut_value) %>%
      do(data.frame(bp_stat=c("low_wisk","pct25","pct50","pct75","high_wisk"),
                    bp_value=boxplot.stats(.$`Industry Price Change (%)` )$stats)) %>%
      mutate(
        shareOutThresh = as.integer(thresh*100)) %>% spread(key=bp_stat,value=bp_value)


  }

}

indicboxdataPooled <- do.call("rbind",lapply(seq(.2,.7,.1),calcBoxStatsInd))
indicboxdataModel <- do.call("rbind",lapply(seq(.2,.7,.1),calcBoxStatsInd,pooled=FALSE))
indicboxdata <- rbind(indicboxdataPooled,indicboxdataModel)
indicboxmktCnt <-  do.call("rbind",lapply(seq(.2,.7,.1),calcBoxStatsInd,reportN=TRUE))

## summarize simulation outcomes
sumdata <- ungroup(res_flat) %>% select(supply,demand,`Industry Price Change (%)`,
                                        `Merging Party Price Change (%)`,`Consumer Harm ($)`, `Producer Benefit ($)`,
                                        `Difference ($)`,shareOut)   %>%
  rename(Supply=supply, Demand = demand,"Net Harm ($)" = `Difference ($)`)


sumdata <- gather(sumdata, Outcome, value, -Supply, -Demand, -shareOut,factor_key = TRUE)



calcBoxStatsSum <- function(thresh=0.1,reportN=FALSE){

  if(reportN){


    res <-   filter(sumdata ,shareOut >= thresh & !is.na(value) ) %>% select(Supply,Demand,Outcome) %>%
        group_by(Outcome) %>% summarise(Cnt=n()) %>%
        mutate(
          shareOutThresh = as.integer(thresh*100))

  }

else{
  res <- filter(sumdata ,shareOut < thresh & !is.na(value) ) %>% select(Supply,Demand,Outcome,value) %>%
    group_by(Supply,Demand,Outcome) %>%
    do(data.frame(bp_stat=c("low_wisk","pct25","pct50","pct75","high_wisk"),
                  bp_value=boxplot.stats(.$value )$stats)) %>%
    mutate(
      shareOutThresh = as.integer(thresh*100))%>% spread(key=bp_stat,value=bp_value)
}

  res <- mutate(res, Model = interaction(Supply, Demand, sep = ":"),
         Model = factor(Model, levels = c("cournot:log", "cournot:linear", "bertrand:aids", "bertrand:logit", "bertrand:ces", "auction:logit")))

}

sumboxdata <- lapply(seq(.2,.7,.1),calcBoxStatsSum)
sumboxdata <- do.call("rbind",sumboxdata)
sumboxmktCnt <-  do.call("rbind",lapply(seq(.2,.7,.1),calcBoxStatsSum,reportN=TRUE))

#ggplot(filter(indicboxdata,Cut_type=="CMCR" & Supply == "Pooled" & shareOutThresh == 30),
#       aes(x=Cut_value,ymin=low_wisk,lower=pct25,middle=pct50,upper=pct75,ymax=high_wisk))+geom_boxplot(stat = "identity")




usethis::use_data(sumboxdata, indicboxdata, sumboxmktCnt ,indicboxmktCnt ,overwrite = TRUE)
