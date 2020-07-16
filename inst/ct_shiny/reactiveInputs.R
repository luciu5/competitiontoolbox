
# Supply
supply <- reactive({

  if (req(input$menu) == "Horizontal"){
    return(input$supply)
  }
})

supplyTariff <- reactive({

  if (req(input$menu) == "Tariffs"){
    return(input$supplyTariffs)
  }
})

supplyQuota <- reactive({

  if (req(input$menu) == "Quotas"){
    return(input$supplyQuota)
  }
})

# Demand
demand <- reactive({

  if (req(input$menu) == "Horizontal"){

    if (input$supply == "Bertrand" & grepl('elasticity', input$calcElast)){
      return(input$demand1)
    }
    if (input$supply == "Bertrand" & !grepl('elasticity', input$calcElast)){
      return(input$demand2)
    }
    if (input$supply == "2nd Score Auction" & grepl('elasticity', input$calcElast)){
      return(input$demand3)
    }
    if (input$supply == "2nd Score Auction" & !grepl('elasticity', input$calcElast)){
      return(input$demand4)
    }
    if (input$supply == "Cournot" & grepl('elasticity', input$calcElast)){
      return(input$demand5)
    }
    if (input$supply == "Cournot" & !grepl('elasticity', input$calcElast)){
      return(input$demand6)
    }
  }
})

demandTariff <- reactive({

  if (req(input$menu) == "Tariffs"){

    if (input$supplyTariffs == "Bertrand" & grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs1)
    }
    if (input$supplyTariffs == "Bertrand" & !grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs2)
    }
    if (input$supplyTariffs == "Cournot" & grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs3)
    }
    if (input$supplyTariffs == "Cournot" & !grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs4)
    }
  }
})

demandQuota <- reactive({

  if (req(input$menu) == "Quotas"){

    if (input$supplyQuota == "Bertrand" & grepl('elasticity', input$calcElastQuota)){
      return(input$demandQuota1)
    }
    if (input$supplyQuota == "Bertrand" & !grepl('elasticity', input$calcElastQuota)){
      return(input$demandQuota2)
    }
  }
})

# Elasticity
elasticity <- reactive({

  if (req(input$menu) == "Horizontal"){

    if (grepl('elasticity', input$calcElast)){
      return(input$enterElast)
    } else {
      return(NA_real_)
    }
  }
})

elasticityTariff <- reactive({

  if (req(input$menu) == "Tariffs"){

    if (grepl('elasticity', input$calcElastTariffs)){
      return(input$enterElastTariffs)
    } else {
      return(NA_real_)
    }
  }
})

elasticityQuota <- reactive({

  if (req(input$menu) == "Quotas"){

    if (grepl('elasticity', input$calcElastQuota)){
      return(input$enterElastQuota)
    } else {
      return(NA_real_)
    }
  }
})
