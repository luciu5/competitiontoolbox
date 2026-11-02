
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
    key <- paste(input$supply, model_has_known_elasticity(input$calcElast), sep = "|")
    demand_input <- c(
      "Bertrand|TRUE" = "demand1",
      "Bertrand|FALSE" = "demand2",
      "2nd Score Auction|TRUE" = "demand3",
      "2nd Score Auction|FALSE" = "demand4",
      "Cournot|TRUE" = "demand5",
      "Cournot|FALSE" = "demand6"
    )[key]
    return(input[[demand_input]])
  }

  if (req(input$menu) == "Vertical") {
    demand_input <- c(
      "Bertrand" = "demandVertical1",
      "2nd Score Auction" = "demandVertical2"
    )[input$supplyVertical]
    return(input[[demand_input]])
  }

  if (req(input$menu) == "Tariffs"){
    key <- paste(input$supplyTariffs, model_has_known_elasticity(input$calcElastTariffs), sep = "|")
    demand_input <- c(
      "Bertrand|TRUE" = "demandTariffs1",
      "Bertrand|FALSE" = "demandTariffs2",
      "Cournot|TRUE" = "demandTariffs3",
      "Cournot|FALSE" = "demandTariffs4",
      "Monopolistic Competition|TRUE" = "demandTariffs5",
      "Monopolistic Competition|FALSE" = "demandTariffs6"
    )[key]
    return(input[[demand_input]])
  }

  if (req(input$menu) == "Quotas"){
    key <- paste(input$supplyQuota, model_has_known_elasticity(input$calcElastQuota), sep = "|")
    demand_input <- c(
      "Bertrand|TRUE" = "demandQuota1",
      "Bertrand|FALSE" = "demandQuota2"
    )[key]
    return(input[[demand_input]])
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
