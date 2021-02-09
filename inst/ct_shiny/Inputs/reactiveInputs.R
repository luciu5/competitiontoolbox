
## Supply
supply <- reactive({

  if (req(input$menu) == "Horizontal"){
    return(input$supply)
  }

  if (req(input$menu) == "Vertical"){
    return(input$supplyVertical)
  }

  if (req(input$menu) == "Tariffs"){
    return(input$supplyTariffs)
  }

  if (req(input$menu) == "Quotas"){
    return(input$supplyQuota)
  }
})


## Demand
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

  if (req(input$menu) == "Vertical") {
    if (input$supplyVertical == "Bertrand") {
      return(input$demandVertical1)
    }
    if (input$supplyVertical == "2nd Score Auction"){
      return(input$demandVertical2)
    }
  }

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
    if (input$supplyTariffs == "Monopolistic Competition" & grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs5)
    }
    if (input$supplyTariffs == "Monopolistic Competition" & !grepl('elasticity', input$calcElastTariffs)){
      return(input$demandTariffs6)
    }
  }

  if (req(input$menu) == "Quotas"){
    if (input$supplyQuota == "Bertrand" & grepl('elasticity', input$calcElastQuota)){
      return(input$demandQuota1)
    }
    if (input$supplyQuota == "Bertrand" & !grepl('elasticity', input$calcElastQuota)){
      return(input$demandQuota2)
    }
  }
})


## Elasticity
elasticity <- reactive({

  if (req(input$menu) == "Horizontal"){
    if (grepl('elasticity', input$calcElast)){
      return(input$enterElast)
    } else {
      return(NA_real_)
    }
  }

  if (req(input$menu) == "Tariffs"){
    if (grepl('elasticity', input$calcElastTariffs)){
      return(input$enterElastTariffs)
    } else {
      return(NA_real_)
    }
  }

  if (req(input$menu) == "Quotas"){
    if (grepl('elasticity', input$calcElastQuota)){
      return(input$enterElastQuota)
    } else {
      return(NA_real_)
    }
  }
})
