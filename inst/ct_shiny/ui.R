## Load libraries
require(competitiontoolbox)
require(ggplot2)

source(file.path(getwd(), "R/modelRegistry.R"))

## Sponsor footer for Shiny interface
logoURL <- "https://CRAN.R-project.org/package=antitrust"
logoSrc <- "logo.png"
logoAlt <- "competitiontoolbox sponsor logo"

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#185a7d",
  secondary = "#5f6f7a",
  success = "#2e7d5b",
  danger = "#a13d3d",
  base_font = "Arial"
)

app_css <- tags$style(HTML("
  body {
    background: #f6f8fa;
  }
  .navbar-brand {
    font-weight: 700;
  }
  .ct-page {
    padding-block: 1rem 1.5rem;
  }
  .ct-title {
    margin: 0 0 1rem;
    font-size: 1.75rem;
    font-weight: 700;
  }
  .ct-section-title {
    margin: 0 0 .75rem;
    font-size: 1.25rem;
    font-weight: 700;
  }
  .ct-sidebar h5 {
    margin-top: 0;
  }
  .ct-sponsor {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: .5rem;
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid #d9e1e8;
    color: #5f6f7a;
    font-size: .85rem;
  }
  .ct-sponsor img {
    height: 40px;
  }
  .ct-action-row {
    display: flex;
    justify-content: flex-end;
    margin-top: .75rem;
  }
  .ct-table-card .card-body {
    overflow-x: auto;
  }
  .ct-note,
  .ct-description {
    margin-top: 1rem;
    padding: .75rem 1rem;
    border-left: 4px solid #9ab3c5;
    background: #eef4f8;
    color: #344552;
  }
  .ct-message-warning {
    color: #9a6700;
  }
  .ct-message-error {
    color: #b42318;
  }
  .ct-plot-output {
    min-height: 520px;
  }
  pre.shiny-text-output {
    white-space: pre-wrap;
  }
"))

sponsor_footer <- function() {
  tags$div(
    class = "ct-sponsor",
    tags$span("Supported by"),
    tags$a(
      href = logoURL,
      target = "_blank",
      tags$img(src = logoSrc, alt = logoAlt)
    )
  )
}

run_button <- function(inputId, label = "Simulate") {
  actionButton(
    inputId = inputId,
    label = label,
    icon = icon("play"),
    class = "btn-primary"
  )
}

note_box <- function(...) {
  tags$div(class = "ct-note", ...)
}

description_box <- function(outputId) {
  tags$div(
    class = "ct-description",
    h5(tags$b("Description:")),
    textOutput(outputId)
  )
}

simulation_layout <- function(title, sidebar, input_output, simulate_id, result_tabs) {
  tags$div(
    class = "ct-page",
    h1(class = "ct-title", title),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        class = "ct-sidebar",
        width = 330,
        sidebar,
        sponsor_footer()
      ),
      bslib::card(
        class = "ct-table-card",
        bslib::card_header("Enter Inputs"),
        input_output,
        tags$div(class = "ct-action-row", run_button(simulate_id))
      ),
      result_tabs
    )
  )
}

horizontal_results_tabs <- function() {
  bslib::navset_card_tab(
    id = "inTabset",
    bslib::nav_panel(
      "Summary",
      value = "respanel",
      tableOutput("results"),
      note_box(
        tags$b("Note: "),
        "All price changes as well as compensating marginal cost reduction are (post-merger) share-weighted averages. ",
        "A negative Consumer Harm number denotes benefit, while a negative Producer Benefit number denotes harm. ",
        "Numbers in parentheses denote harm and benefit as a percentage of post-merger revenues."
      )
    ),
    bslib::nav_panel(
      "Details",
      value = "detpanel",
      tableOutput("results_shareOut"),
      tableOutput("results_detailed")
    ),
    bslib::nav_panel(
      "Elasticities",
      value = "elastpanel",
      radioButtons("pre_elast", "", choices = c("Pre-Merger", "Post-Merger"), inline = TRUE),
      tableOutput("results_mktelast"),
      tableOutput("results_elast"),
      conditionalPanel(
        "input.supply != 'Cournot'",
        checkboxInput("diversions", "Report diversion ratios", value = FALSE),
        note_box(
          tags$b("Note: "),
          "Diagonal elements are own-price elasticities. Off-diagonal elements are cross-price elasticities of row with respect to column."
        )
      ),
      conditionalPanel(
        "input.supply == 'Cournot'",
        note_box(tags$b("Note: "), "Above are own-price elasticities.")
      )
    ),
    bslib::nav_panel(
      "Diagnostics",
      value = "diagpanel",
      h4("Percent Differences between Inputted and Fitted Values Relative to Inputs"),
      tableOutput("results_diag_elast"),
      tableOutput("results_diagnostics"),
      htmlOutput("overIDText"),
      h4("Parameters"),
      verbatimTextOutput("parameters"),
      helpText("See the", tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the parameters displayed here.")
    ),
    bslib::nav_panel("R Code", value = "codepanel", verbatimTextOutput("results_code")),
    bslib::nav_panel(
      "Messages",
      value = "msgpanel",
      h4("Warnings"),
      span(class = "ct-message-warning", textOutput("warnings")),
      h4("Errors"),
      span(class = "ct-message-error", textOutput("errors"))
    ),
    title = "Results"
  )
}

vertical_results_tabs <- function() {
  bslib::navset_card_tab(
    id = "inTabsetVertical",
    bslib::nav_panel(
      "Summary",
      value = "respanelVertical",
      tableOutput("resultsVertical"),
      note_box(
        tags$b("Note: "),
        "All price changes as well as compensating marginal cost reduction are (post-merger) share-weighted averages. ",
        "A negative Consumer Harm number denotes benefit, while a negative Producer Benefit number denotes harm."
      )
    ),
    bslib::nav_panel(
      "Details",
      value = "detpanelVertical",
      tableOutput("results_shareOutVertical"),
      tableOutput("results_detailedVertical")
    ),
    bslib::nav_panel(
      "Elasticities",
      value = "elastpanelVertical",
      radioButtons("pre_elastVertical", "", choices = c("Pre-Merger", "Post-Merger"), inline = TRUE),
      tableOutput("results_mktelastVertical"),
      tableOutput("results_elastVertical"),
      conditionalPanel(
        "input.supplyVertical != 'Cournot'",
        checkboxInput("diversionsVertical", "Report diversion ratios", value = FALSE),
        note_box(
          tags$b("Note: "),
          "Diagonal elements are own-price elasticities. Off-diagonal elements are cross-price elasticities of row with respect to column."
        )
      ),
      conditionalPanel(
        "input.supplyVertical == 'Cournot'",
        note_box(tags$b("Note: "), "Above are own-price elasticities.")
      )
    ),
    bslib::nav_panel(
      "Diagnostics",
      value = "diagpanelVertical",
      h4("Percent Differences between Inputted and Fitted Values Relative to Inputs"),
      tableOutput("results_diag_elastVertical"),
      tableOutput("results_diagnosticsVertical"),
      htmlOutput("overIDTextVertical"),
      h4("Parameters"),
      verbatimTextOutput("parametersVertical"),
      helpText("See the", tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the parameters displayed here.")
    ),
    bslib::nav_panel("R Code", value = "codepanelVertical", verbatimTextOutput("results_codeVertical")),
    bslib::nav_panel(
      "Messages",
      value = "msgpanelVertical",
      h4("Warnings"),
      span(class = "ct-message-warning", textOutput("warningsVertical")),
      h4("Errors"),
      span(class = "ct-message-error", textOutput("errorsVertical"))
    ),
    title = "Results"
  )
}

trade_results_tabs <- function(type) {
  is_tariff <- identical(type, "Tariffs")
  suffix <- if (is_tariff) "Tariffs" else "Quota"
  id <- if (is_tariff) "inTabsetTariffs" else "inTabsetQuota"
  res <- if (is_tariff) "respanelTariffs" else "respanelQuota"
  det <- if (is_tariff) "detpanelTariffs" else "detpanelQuota"
  elast_panel <- if (is_tariff) "elastpanelTariffs" else "elastpanelQuota"
  diag <- if (is_tariff) "diagpanelTariffs" else "diagpanelQuota"
  code <- if (is_tariff) "codepanelTariffs" else "codepanelQuota"
  msg <- if (is_tariff) "msgpanelTariffs" else "msgpanelQuota"
  current_label <- if (is_tariff) "Current Tariff" else "Current Quota"
  new_label <- if (is_tariff) "New Tariff" else "New Quota"
  instrument <- if (is_tariff) "new tariff" else "new quota"
  supply_input <- if (is_tariff) "input.supplyTariffs" else "input.supplyQuota"

  bslib::navset_card_tab(
    id = id,
    bslib::nav_panel(
      "Summary",
      value = res,
      tableOutput(paste0("results", suffix)),
      note_box(
        tags$b("Note: "),
        "All price changes are (", instrument, ") share-weighted averages. Negative Consumer Harm or Net Harm numbers denote benefit."
      )
    ),
    bslib::nav_panel(
      "Details",
      value = det,
      tableOutput(paste0("results_shareOut", suffix)),
      tableOutput(paste0("results_detailed", suffix))
    ),
    bslib::nav_panel(
      "Elasticities",
      value = elast_panel,
      radioButtons(paste0("pre_elast", suffix), "", choices = c(current_label, new_label), inline = TRUE),
      tableOutput(paste0("results_mktelast", suffix)),
      tableOutput(paste0("results_elast", suffix)),
      conditionalPanel(
        paste0(supply_input, " !== 'Cournot'"),
        checkboxInput(paste0("diversions", suffix), "Report diversion ratios", value = FALSE),
        note_box(
          tags$b("Note: "),
          "Diagonal elements are own-price elasticities. Off-diagonal elements are cross-price elasticities of row with respect to column."
        )
      ),
      conditionalPanel(
        paste0(supply_input, " == 'Cournot'"),
        note_box(tags$b("Note: "), "Above are own-price elasticities.")
      )
    ),
    bslib::nav_panel(
      "Diagnostics",
      value = diag,
      h4("Inputted vs. Fitted Values"),
      tableOutput(paste0("results_diag_elast", suffix)),
      tableOutput(paste0("results_diagnostics", suffix)),
      htmlOutput(paste0("overIDText", suffix)),
      h4("Parameters"),
      verbatimTextOutput(paste0("parameters", suffix)),
      helpText("See the", tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the parameters displayed here.")
    ),
    bslib::nav_panel("R Code", value = code, verbatimTextOutput(paste0("results_code", suffix))),
    bslib::nav_panel(
      "Messages",
      value = msg,
      h4("Warnings"),
      verbatimTextOutput(paste0("warnings", suffix)),
      h4("Errors"),
      verbatimTextOutput(paste0("errors", suffix))
    ),
    title = "Results"
  )
}

horizontal_merger_page <- function() {
  simulation_layout(
    "Simulate a Horizontal Merger",
    tagList(
      htmlOutput("urlText"),
      hr(),
      h5(tags$b("Directions:")),
      helpText(tags$ul(
        tags$li("Copy and paste or manually enter market data into the Inputs table."),
        tags$li("Click Simulate to simulate a merger between 'Firm1' and 'Firm2'."),
        tags$li("See the", tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the models used.")
      )),
      hr(),
      radioButtons("calcElast", "Calibrate model parameters using:", choices = c("market elasticity and 1 or more margins", "2 or more margins")),
      conditionalPanel(
        condition = "input.calcElast.includes('elasticity') == true",
        numericInput("enterElast", "Enter Market Elasticity:", value = -1, min = -Inf, max = 0, step = .1)
      ),
      hr(),
      radioButtons("supply", "Competitive Interaction:", choices = c("Bertrand", "2nd Score Auction", "Cournot")),
      conditionalPanel(
        condition = "input.supply == 'Bertrand' & input.calcElast.includes('elasticity') == true",
        selectInput("demand1", "Demand Specification:", choices = model_demand_choices("Horizontal", "Bertrand", TRUE))
      ),
      conditionalPanel(
        condition = "input.supply == 'Bertrand' & input.calcElast.includes('elasticity') == false",
        selectInput("demand2", "Demand Specification:", choices = model_demand_choices("Horizontal", "Bertrand", FALSE))
      ),
      conditionalPanel(
        condition = "input.supply == '2nd Score Auction' & input.calcElast.includes('elasticity') == true",
        selectInput("demand3", "Demand Specification:", choices = model_demand_choices("Horizontal", "2nd Score Auction", TRUE)),
        helpText(tags$b("Note:"), "2nd Score Auction only requires a single price.")
      ),
      conditionalPanel(
        condition = "input.supply == '2nd Score Auction' & input.calcElast.includes('elasticity') == false",
        selectInput("demand4", "Demand Specification:", choices = model_demand_choices("Horizontal", "2nd Score Auction", FALSE)),
        helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
      ),
      conditionalPanel(
        condition = "input.supply == 'Cournot' & input.calcElast.includes('elasticity') == true",
        selectInput("demand5", "Demand Specification:", choices = model_demand_choices("Horizontal", "Cournot", TRUE)),
        helpText(tags$b("Note:"), "Linear and loglinear Cournot use only the first non-missing inputted price and product name.")
      ),
      conditionalPanel(
        condition = "input.supply == 'Cournot' & input.calcElast.includes('elasticity') == false",
        selectInput("demand6", "Demand Specification:", choices = model_demand_choices("Horizontal", "Cournot", FALSE)),
        helpText(tags$b("Note:"), "Linear and loglinear Cournot use only the first non-missing inputted price and product name.")
      ),
      conditionalPanel(
        condition = "input.supply == 'Bertrand' & input.demand1.includes('aids') == true & input.calcElast.includes('elasticity') == true",
        helpText(tags$b("Note:"), "'aids' does not require pricing information.")
      ),
      conditionalPanel(
        condition = "input.supply == 'Bertrand' & input.demand2.includes('aids') == true & input.calcElast.includes('elasticity') == false",
        helpText(tags$b("Note:"), "'aids' does not require pricing information.")
      )
    ),
    rHandsontableOutput("hot"),
    "simulate",
    horizontal_results_tabs()
  )
}

vertical_merger_page <- function() {
  simulation_layout(
    "Simulate a Merger in a Supply Chain",
    tagList(
      htmlOutput("urlTextVertical"),
      hr(),
      h5(tags$b("Directions:")),
      helpText(tags$ul(
        tags$li("Copy and paste or manually enter market data into the Inputs table."),
        tags$li(htmlOutput("directionsVertical")),
        tags$li("See the", tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the models used.")
      )),
      hr(),
      sliderInput("addRowsVertical", "Add rows to Inputs table:", value = 10, min = 5, max = 50, step = 5),
      selectInput("mergerTypeVertical", "Merger Type:", choices = c("Upstream", "Downstream", "Vertical")),
      radioButtons("supplyVertical", "Competitive Interaction:", choices = c("Bertrand", "2nd Score Auction")),
      conditionalPanel(
        condition = "input.supplyVertical == 'Bertrand'",
        selectInput("demandVertical1", "Downstream Demand Specification:", choices = model_demand_choices("Vertical", "Bertrand", TRUE)),
        helpText(tags$b("Note:"), "Share of outside good implied by the sum of inside product shares. Price of outside good fixed at 0.")
      ),
      conditionalPanel(
        condition = "input.supplyVertical == '2nd Score Auction'",
        selectInput("demandVertical2", "Downstream Demand Specification:", choices = model_demand_choices("Vertical", "2nd Score Auction", TRUE)),
        helpText(tags$b("Note:"), "Share of outside good implied by the sum of inside product shares. Price of outside good fixed at 0.")
      )
    ),
    rHandsontableOutput("hotVertical"),
    "simulateVertical",
    vertical_results_tabs()
  )
}

tariffs_page <- function() {
  simulation_layout(
    "Simulate a Tariff",
    tagList(
      htmlOutput("urlTextTariffs"),
      hr(),
      h5(tags$b("Directions:")),
      helpText(tags$ul(
        tags$li("Copy and paste or manually enter market data into the Inputs table."),
        tags$li("Click Simulate to simulate an", tags$em("ad valorem"), "tariff."),
        tags$li("Default example simulates an increase in the ", tags$em("ad valorem"), "tariff from 5% to 25% on products produced by 'Firm1' and 'Firm2'."),
        tags$li("Products without current or new tariffs are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
      )),
      hr(),
      sliderInput("addRowsTariffs", "Add rows to Inputs table:", value = 10, min = 5, max = 50, step = 5),
      radioButtons("calcElastTariffs", "Calibrate model parameters using:", choices = c("market elasticity AND 1 or more margins", "2 or more margins")),
      conditionalPanel(
        condition = "input.calcElastTariffs.includes('elasticity') == true ",
        numericInput("enterElastTariffs", "Enter Market Elasticity:", value = -1, min = -Inf, max = 0, step = .1)
      ),
      hr(),
      radioButtons("supplyTariffs", "Competitive Interaction:", choices = c("Bertrand", "Monopolistic Competition", "Cournot")),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Bertrand' & input.calcElastTariffs.includes('elasticity') == true",
        selectInput("demandTariffs1", "Demand Specification:", choices = model_demand_choices("Tariffs", "Bertrand", TRUE))
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Bertrand' & input.calcElastTariffs.includes('elasticity') == false",
        selectInput("demandTariffs2", "Demand Specification:", choices = model_demand_choices("Tariffs", "Bertrand", FALSE))
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Cournot' & input.calcElastTariffs.includes('elasticity') == true",
        selectInput("demandTariffs3", "Demand Specification:", choices = model_demand_choices("Tariffs", "Cournot", TRUE)),
        helpText(tags$b("Note:"), "Linear and loglinear Cournot use only the first non-missing inputted price and product name.")
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Cournot' & input.calcElastTariffs.includes('elasticity') == false",
        selectInput("demandTariffs4", "Demand Specification:", choices = model_demand_choices("Tariffs", "Cournot", FALSE)),
        helpText(tags$b("Note:"), "Linear and loglinear Cournot use only the first non-missing inputted price and product name.")
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Monopolistic Competition' & input.calcElastTariffs.includes('elasticity') == true",
        selectInput("demandTariffs5", "Demand Specification:", choices = model_demand_choices("Tariffs", "Monopolistic Competition", TRUE))
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Monopolistic Competition' & input.calcElastTariffs.includes('elasticity') == false",
        selectInput("demandTariffs6", "Demand Specification:", choices = model_demand_choices("Tariffs", "Monopolistic Competition", FALSE))
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Bertrand' && input.calcElastTariffs.includes('elasticity') == true && input.demandTariffs1 == 'aids'",
        helpText(tags$b("Note:"), "aids does not require pricing information.")
      ),
      conditionalPanel(
        condition = "input.supplyTariffs == 'Bertrand' && input.calcElastTariffs.includes('elasticity') == false && input.demandTariffs2 == 'aids (unknown elasticity)'",
        helpText(tags$b("Note:"), "aids does not require pricing information.")
      )
    ),
    rHandsontableOutput("hotTariffs"),
    "simulateTariffs",
    trade_results_tabs("Tariffs")
  )
}

quotas_page <- function() {
  simulation_layout(
    "Simulate a Quota",
    tagList(
      htmlOutput("urlTextQuota"),
      hr(),
      h5(tags$b("Directions:")),
      helpText(tags$ul(
        tags$li("Copy and paste or manually enter market data into the Inputs table."),
        tags$li("Click Simulate to simulate a quota."),
        tags$li("Default example simulates an increase in the quota from 100% of current output to 75% of current output on products produced by 'Firm1' and 'Firm2'."),
        tags$li("Products without current or new quotas are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
      )),
      hr(),
      sliderInput("addRowsQuota", "Add rows to Inputs table:", value = 10, min = 5, max = 50, step = 5),
      radioButtons("calcElastQuota", "Calibrate model parameters using:", choices = c("market elasticity and 1 or more margins", "2 or more margins"), selected = "market elasticity and 1 or more margins"),
      conditionalPanel(
        condition = "input.calcElastQuota.includes('elasticity') == true ",
        numericInput("enterElastQuota", "Enter Market Elasticity:", value = -1, min = -Inf, max = 0, step = .1)
      ),
      hr(),
      radioButtons("supplyQuota", "Competitive Interaction:", choices = c("Bertrand")),
      conditionalPanel(
        condition = "input.supplyQuota == 'Bertrand' & input.calcElastQuota.includes('elasticity') == true",
        selectInput("demandQuota1", "Demand Specification:", choices = model_demand_choices("Quotas", "Bertrand", TRUE))
      ),
      conditionalPanel(
        condition = "input.supplyQuota == 'Bertrand' & input.calcElastQuota.includes('elasticity') == false",
        selectInput("demandQuota2", "Demand Specification:", choices = model_demand_choices("Quotas", "Bertrand", FALSE))
      )
    ),
    rHandsontableOutput("hotQuota"),
    "simulateQuota",
    trade_results_tabs("Quotas")
  )
}

introduction_page <- function() {
  tags$div(
    class = "ct-page",
    bslib::card(
      h1(class = "ct-title", HTML("Welcome to the <em>competitiontoolbox</em> Shiny App")),
      h3("Overview"),
      p(
        "The",
        tags$a(href = "https://CRAN.R-project.org/package=competitiontoolbox", "competitiontoolbox", target = "_blank"),
        "Shiny application is a browser-based user interface for functionality embedded in the",
        tags$a(href = "https://CRAN.R-project.org/package=antitrust", "antitrust", target = "_blank"),
        "and",
        tags$a(href = "https://CRAN.R-project.org/package=trade", "trade", target = "_blank"),
        "R packages."
      ),
      tags$ul(
        tags$li("Simulate mergers, tariffs, and quotas under various specifications and market conditions."),
        tags$li("Numerically simulate horizontal and vertical mergers."),
        tags$li("Visualize estimated effects on prices, surplus, market structure, and related outcomes.")
      ),
      p("Users may input simulation parameters in the sidebars and edit market conditions in the input tables, including prices, margins, and shares."),
      p(
        "To better understand the types of predictions that these models make, users may also view the distribution of outcomes from numerical simulations. See",
        tags$a(href = "https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers", "Taragin and Loudermilk (2019)", target = "_blank"),
        "and",
        tags$a(href = "https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining", "Sheu and Taragin (2020)", target = "_blank"),
        "for more details."
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        h3("Get Started"),
        p("Use Mergers > Horizontal to simulate a horizontal merger."),
        p("Use Mergers > Vertical to simulate a merger in a supply chain."),
        p("Use Trade > Tariffs to simulate a tariff."),
        p("Use Trade > Quotas to simulate a quota.")
      ),
      bslib::card(
        h3("Output Tabs"),
        tags$ul(
          tags$li(tags$em("Summary"), ": summary statistics including HHI, surplus, and price changes."),
          tags$li(tags$em("Details"), ": product-level prices, shares, and cost reductions."),
          tags$li(tags$em("Elasticities"), ": elasticity and diversion matrices."),
          tags$li(tags$em("Diagnostics"), ": fitted versus inputted values and model parameters."),
          tags$li(tags$em("R Code"), ": reproducible R code for the selected simulation."),
          tags$li(tags$em("Messages"), ": warnings and errors from the simulation.")
        )
      )
    ),
    sponsor_footer()
  )
}

documentation_page <- function(package) {
  tags$div(
    class = "ct-page",
    bslib::card(
      h1(class = "ct-title", paste(package, "Documentation")),
      p(
        "Open the current CRAN reference material for the",
        package,
        "package."
      ),
      tags$a(
        class = "btn btn-primary",
        href = sprintf("https://cran.r-project.org/web/packages/%s/vignettes/Reference.html", package),
        target = "_blank",
        "Open Documentation"
      )
    )
  )
}

numerical_sidebar <- function(..., output = NULL) {
  tagList(
    h5(tags$b("Overview:")),
    helpText(tags$ul(...)),
    output,
    sponsor_footer()
  )
}

horizontal_numerical_page <- function() {
  tags$div(
    class = "ct-page",
    h1(class = "ct-title", "Horizontal Simulations"),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Summary",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 330,
            numerical_sidebar(
              tags$li(htmlOutput("sumNumMergerATR")),
              tags$li(helpText("See ", tags$a(href = "https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers", "Taragin and Loudermilk (2019)"), "for further details.")),
              output = tagList(
                selectInput("outcomeSumATR", "Outcome to Report:", choices = c("Consumer Harm ($)", "Producer Benefit ($)", "Net Harm ($)", "Industry Price Change (%)", "Merging Party Price Change (%)")),
                sliderInput("shareOutSumATR", "Restrict Market by Outside Share (%):", value = 30, min = 10, max = 60, step = 10)
              )
            )
          ),
          plotOutput("plotSumATR", height = "560px"),
          description_box("capSumATR")
        )
      ),
      bslib::nav_panel(
        "Indices",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 330,
            numerical_sidebar(
              tags$li(htmlOutput("indicNumMergerATR")),
              tags$li(helpText("See ", tags$a(href = "https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers", "Taragin and Loudermilk (2019)"), "for further details.")),
              output = tagList(
                radioButtons("pooledIndATR", "Plot Display:", choices = c("Pooled", "By Demand Model"), selected = "Pooled"),
                selectInput("indexIndATR", "Index:", choices = c("Firm Count", "HHI", "Delta HHI", "UPP", "CMCR", "Harm2nd")),
                sliderInput("shareOutIndATR", "Restrict Market by Outside Share (%):", value = 30, min = 10, max = 60, step = 10)
              )
            )
          ),
          plotOutput("plotIndATR", height = "560px"),
          description_box("capIndATR")
        )
      )
    )
  )
}

vertical_numerical_page <- function() {
  vertical_panel <- function(title, outputId, captionId, input = NULL) {
    bslib::nav_panel(
      title,
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 330,
          numerical_sidebar(
            tags$li(helpText("See ", tags$a(href = "https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining", "Sheu and Taragin (2020)"), "for further details.")),
            output = input
          )
        ),
        imageOutput(outputId, width = "100%", height = "560px"),
        description_box(captionId)
      )
    )
  }

  tags$div(
    class = "ct-page",
    h1(class = "ct-title", "Vertical Simulations"),
    bslib::navset_card_tab(
      vertical_panel("Summary", "figSummary", "capSummary"),
      vertical_panel("Upstream", "figUpstream", "capUpstream", radioButtons("upstreamPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter")),
      vertical_panel("Downstream", "figDownstream", "capDownstream", radioButtons("downstreamPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter")),
      vertical_panel("Vertical", "figVertical", "capVertical", radioButtons("verticalPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter"))
    )
  )
}

tariff_numerical_page <- function() {
  tags$div(
    class = "ct-page",
    h1(class = "ct-title", "Tariff Simulations"),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Summary",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 330,
            numerical_sidebar(
              tags$li(htmlOutput("sumNumMergerTariffs")),
              tags$li(helpText("See ", tags$a(href = "https://www.researchgate.net/publication/330564982_Using_measures_of_competitive_harm_for_optimal_screening_of_horizontal_mergers", "Taragin and Loudermilk (2019)"), "for further details.")),
              output = tagList(
                selectInput("outcomeSumTariffs", "Outcome to Report:", choices = c("Domestic Firm Benefit", "Domestic Firm Price Change", "Foreign Firm Harm", "Foreign Firm Price Change", "Industry Price Change", "Consumer Harm", "Net Domestic Harm", "Net Total Harm")),
                sliderInput("tariffThreshSum", "Restrict Market by Tariff (%):", value = 20, min = 10, max = 30, step = 10)
              )
            )
          ),
          plotOutput("plotSumTariffs", height = "560px"),
          description_box("capSumTariffs")
        )
      )
    )
  )
}

other_resources_page <- function() {
  tags$div(
    class = "ct-page",
    bslib::card(
      h1(class = "ct-title", "Additional Research"),
      p(
        "Luke Froeb and Steven Tschantz (Vanderbilt University) have developed a ",
        tags$a(href = "https://daag.shinyapps.io/b1x2", "vertical merger simulator app", target = "_blank"),
        " that allows users to compare simulated vertical merger effects across different bargaining models, including Nash-in-Nash two-part pricing and various models of derived demand."
      ),
      p(
        "This vertical simulator accompanies ",
        tags$a(href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3760634", "Boshoff, Froeb, Minnie, and Tschantz (2020)", target = "_blank", .noWS = "outside"),
        ", which provides theoretical frameworks for the included models."
      )
    ),
    sponsor_footer()
  )
}

bslib::page_navbar(
  title = "competitiontoolbox",
  id = "menu",
  selected = "Introduction",
  theme = app_theme,
  window_title = "competitiontoolbox",
  header = app_css,
  bslib::nav_panel("Introduction", introduction_page()),
  bslib::nav_menu(
    "Mergers",
    bslib::nav_panel("Horizontal", horizontal_merger_page()),
    bslib::nav_panel("Vertical", vertical_merger_page()),
    bslib::nav_panel("Documentation", documentation_page("antitrust"))
  ),
  bslib::nav_menu(
    "Trade",
    bslib::nav_panel("Tariffs", tariffs_page()),
    bslib::nav_panel("Quotas", quotas_page()),
    bslib::nav_panel("Documentation", documentation_page("trade"))
  ),
  bslib::nav_menu(
    "Numerical Simulations",
    bslib::nav_panel("Horizontal Mergers", horizontal_numerical_page()),
    bslib::nav_panel("Vertical Mergers", vertical_numerical_page()),
    bslib::nav_panel("Tariffs", tariff_numerical_page())
  ),
  bslib::nav_panel("Other Resources", other_resources_page())
)
