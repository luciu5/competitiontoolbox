
## Load libraries
require(competitiontoolbox)
require(ggplot2)

## Sponsor Footer for Shiny Interface
logoURL <- "https://cran.r-project.org/web/packages/antitrust/index.html"
logoSrc <- "logo.png"
logoAlt <- "Insert Logo Here"
logo <- HTML(paste(tags$a(href=logoURL, tags$img(src= logoSrc,alt= logoAlt,style='height:40px'))))


navbarPage("", id = "menu",
                   tabPanel("Introduction",
                            fluidPage(
                              titlePanel(div(HTML("Welcome to the <em>competitiontoolbox</em> RShiny App!"))),
                              h3("Overview"),
                              p("The", tags$a(href="https://cran.r-project.org/web/packages/competitiontoolbox/index.html", "competitiontoolbox"), "is a browser-based interface to some of functions in the ", tags$a(href="https://cran.r-project.org/web/packages/antitrust/index.html", "antitrust"), "and",
                                tags$a(href="https://cran.r-project.org/web/packages/trade/index.html", "trade"), "R packages. It allows users to "),
                              HTML("<ul>
                                     <li>simulate mergers, tariffs, and quotas under various specifications and market conditions,</li>
                                     <li>numerically simulate horizontal and vertical mergers,</li>
                                     <li>and visualize the estimated impact these transactions have on various market outcomes.</li>
                                   </ul>"), br(),
                              p("Users may input different simulation parameters on the lefthand-side panels found in the pages linked by the tabs above. These parameters include
                                the assumed competitive environment and market demand system. Users may also edit market conditions listed in the Inputs tables such as firm prices, margins,
                                and shares. Both horizontal and supply chain mergers are available for simulation, including upstream, downstream, and vertical
                                mergers. Example inputs for each type of simulation are provided to users in the corresponding Inputs table."),
                              p("To better understand the types of predictions that these models make, users may also view the distribution of outcomes from thousands of numerical simulations. See", tags$a(href="https://www.researchgate.net/publication/330564982_Using_concentration_measures_for_optimal_screening_of_horizontal_mergers", "Taragin and Loudermilk (2019)"),
                                "and", tags$a(href="https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining", "Sheu and Taragin (2020)"), "for more details."),

                              hr(),
                              h3("Get Started"),
                              p("To simulate a horizontal merger, proceed to", strong("Horizontal"), "listed under the", strong("Mergers"), "tab."),
                              p("To simulate a merger in a supply chain, proceed to", strong("Vertical"), "listed under the", strong("Mergers"), "tab."),
                              br(),
                              p("To numerically simulate a horizontal merger, proceed to", strong("Horizontal"), "listed under the", strong("Numerical Simulations"), "tab."),
                              p("To numerically simulate a merger in a supply chain, proceed to", strong("Vertical"), "listed under the", strong("Numerical Simulations"), "tab."),
                              br(),
                              p("To simulate a tariff, proceed to", strong("Tariffs"), "listed under the", strong("Trade"), "tab."),
                              p("To simulate a quota, proceed to", strong("Quotas"), "listed under the", strong("Trade"), "tab."), br(),

                              p("When run, each simulation outputs a series of tabs which provides detailed information on the simulated merger, tariff,
                                or quota. They are:"),
                              HTML("<ul>
                                      <li><em>Summary</em>: Outputs summary statistics of the simulation, including changes in HHI, consumer and producer surplus, and share-weighted prices.</li>
                                      <li><em>Details</em>: Outputs product-level changes in prices and compensating marginal cost reductions. For supply chain mergers, both upstream and downstream price and share changes are reported.</li>
                                      <li><em>Elasticities</em>: Outputs matrices of estimated elasticities and diversion ratios.</li>
                                      <li><em>Diagnostics</em>: Outputs differences between outputted and fitted values in order to diagnose the simulation. Key underlying parameters are also reported.</li>
                                      <li><em>R Code</em>: Outputs the corresponding R code that runs the inputted simulation. This provides practioners reproducible code as they transition to scripting analyses.</li>
                                      <li><em>Messages</em>: Outputs any error or warning messages encountered by the app.</li>
                                   </ul>")
                            ),
                            hr(),
                            fluidRow(
                              column(width = 12, align = "center",
                                     tags$div(
                                       HTML("<font size=\"2\"> Supported by </font>"),
                                       HTML(logo)
                                     )
                              )
                            )
                    ),

                   navbarMenu("Mergers",
                              tabPanel("Horizontal", style = "overflow-y:scroll; max-height: 90vh",
                                       fluidPage(
                                         titlePanel("Simulate a Horizontal Merger") ,

                                         sidebarLayout(
                                           sidebarPanel(

                                             htmlOutput("urlText"),hr(),

                                             h5(tags$b("Directions:")),
                                             helpText(tags$ul(
                                               tags$li("Copy and paste or manually enter market data into the Inputs table."),
                                               tags$li("Click on the Play button to simulate a merger between 'Firm1' and 'Firm2'."),
                                               tags$li("See the", tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"package vignette for more details about the models used." )
                                               #tags$li("Shares must be between 0 and 1."),
                                               #tags$li("Margins should exclude fixed costs.")
                                             )
                                             ),hr(),
                                             radioButtons("calcElast", "Calibrate model parameters using:",
                                                          choices = c("market elasticity and 1 or more margins",
                                                                      "2 or more margins")
                                             ),
                                             conditionalPanel(
                                               condition = "input.calcElast.includes('elasticity') == true",
                                               numericInput("enterElast", "Enter Market Elasticity:", value = -1, min = -Inf, max = 0, step = .1  #, width='75%'
                                               )
                                             ),
                                             hr(),
                                             #checkboxInput("incEff", "Include Proportional Cost Changes (negative values imply cost reductions)", value = FALSE, width = NULL),

                                             radioButtons("supply", "Competitive Interaction:",
                                                          choices = c("Bertrand",
                                                                      "2nd Score Auction",
                                                                      "Cournot"
                                                          )),

                                             ## Use conditionalPanel() to select appropriate demand forms for each unique pair of competitive interaction and margin information
                                             # Bertrand
                                             conditionalPanel(
                                               condition = "input.supply == 'Bertrand' & input.calcElast.includes('elasticity') == true",
                                               selectInput("demand1", "Demand Specification:",
                                                           choices = c("logit", "ces", "aids"))
                                             ),
                                             conditionalPanel(
                                               condition = "input.supply == 'Bertrand' & input.calcElast.includes('elasticity') == false",
                                               selectInput("demand2", "Demand Specification:",
                                                           choices = c("logit (unknown elasticity)", "ces (unknown elasticity)", "aids (unknown elasticity)"))
                                             ),
                                             # 2nd Score Auction
                                             conditionalPanel(
                                               condition = "input.supply == '2nd Score Auction' & input.calcElast.includes('elasticity') == true",
                                               selectInput("demand3", "Demand Specification:",
                                                           choices = "logit"),
                                               helpText(tags$b("Note:"), "2nd Score Auction only requires a single price.")
                                             ),
                                             conditionalPanel(
                                               condition = "input.supply == '2nd Score Auction' & input.calcElast.includes('elasticity') == false",
                                               selectInput("demand4", "Demand Specification:",
                                                           choices = "logit (unknown elasticity)"),
                                               helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
                                             ),
                                             # Cournot
                                             conditionalPanel(
                                               condition = "input.supply == 'Cournot' & input.calcElast.includes('elasticity') == true",
                                               selectInput("demand5", "Demand Specification:",
                                                           choices = c("linear", "loglinear")),
                                               helpText(tags$b("Note:"), "Only the first non-missing inputted price and product name is used for Cournot.")
                                             ),conditionalPanel(
                                               condition = "input.supply == 'Cournot' & input.calcElast.includes('elasticity') == false",
                                               selectInput("demand6", "Demand Specification:",
                                                           choices = c("linear (unknown elasticity)", "loglinear (unknown elasticity)")),
                                               helpText(tags$b("Note:"), "Only the first non-missing inputted price and product name is used for Cournot.")
                                             ),
                                             # Output additional note for aids/aids (unknown elasticity) demand forms under Bertrand pricing
                                             conditionalPanel(
                                               condition = "input.supply == 'Bertrand' & input.demand1.includes('aids') == true & input.calcElast.includes('elasticity') == true",
                                               helpText(tags$b("Note:"), "'aids' does not require pricing information.")
                                             ),
                                             conditionalPanel(
                                               condition = "input.supply == 'Bertrand' & input.demand2.includes('aids') == true & input.calcElast.includes('elasticity') == false",
                                               helpText(tags$b("Note:"), "'aids' does not require pricing information.")
                                             ),

                                             hr(),
                                             fluidRow(
                                               column(width=12, align = "center",
                                                      tags$div(
                                                        HTML("<font size=\"2\"> Supported by </font>"),
                                                        HTML(logo)
                                                      )
                                               )
                                             )
                                           ),

                                           mainPanel(
                                             h2("Enter Inputs"),
                                             rHandsontableOutput("hot"), br(),
                                             #tags$head(
                                             #  tags$style(HTML('#run{color:white;background-color:black}'))
                                             #),
                                             actionButton(inputId = "simulate", label = "", icon = icon("play"), width = '60px', style='padding:4px')
                                             #)
                                             ,
                                             br(), br(),br(),
                                             tabsetPanel(id = "inTabset",
                                                         tabPanel("Summary", value = "respanel", br(),br(),tableOutput("results"), br(),
                                                                  helpText(tags$b("Note:"), "All price changes as well as compensating marginal cost reduction are (post-merger) share-weighted averages.
                                                                                             A negative Consumer Harm number denotes benefit, while a negative Producer Benefit number denotes harm.
                                                                                             Numbers in parentheses denote harm and benefit as a percentage of post-merger revenues.")
                                                         ),
                                                         tabPanel("Details", value = "detpanel", br(), tableOutput("results_shareOut"), br(), tableOutput("results_detailed")

                                                                  #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                                                                  #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                                                                  #)
                                                         ),
                                                         tabPanel("Elasticities", value = "elastpanel", br(),
                                                                  radioButtons("pre_elast", "",
                                                                               choices = c("Pre-Merger",
                                                                                           "Post-Merger"
                                                                               ), inline = TRUE),
                                                                  br(),
                                                                  tableOutput("results_mktelast"),
                                                                  tableOutput("results_elast"),
                                                                  conditionalPanel("input.supply != 'Cournot'",
                                                                                   checkboxInput("diversions", "Report diversion ratios", value =FALSE),
                                                                                   helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.","Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                                                                  ),
                                                                  conditionalPanel("input.supply == 'Cournot'",
                                                                                   helpText(tags$b("Note:"), "above are own-price elasticities")
                                                                  )
                                                         ),
                                                         tabPanel("Diagnostics", value = "diagpanel", br(), h4("Percent Differences between Inputted and Fitted Values Relative to Inputs"), br(),
                                                                  tableOutput("results_diag_elast"),
                                                                  tableOutput("results_diagnostics"),
                                                                  htmlOutput("overIDText"),br(),
                                                                  #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                                                                  h4("Parameters"),verbatimTextOutput("parameters"),
                                                                  helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"package vignette for more details about the parameters displayed here." )
                                                         ),
                                                         tabPanel("R Code",  value = "codepanel", br(),verbatimTextOutput("results_code")),
                                                         tabPanel("Messages", value = "msgpanel", br(),h4("Warnings"),  span(textOutput("warnings"), style="color:orange"), br(),
                                                                  h4("Errors"),
                                                                  span(textOutput("errors"), style="color:red"))

                                             )

                                           )

                                         )


                                       )
                              ),


                              tabPanel("Vertical", style = "overflow-y:scroll; max-height: 90vh",
                                       fluidPage(
                                         titlePanel("Simulate a Merger in a Supply Chain") ,

                                         sidebarLayout(
                                           sidebarPanel(

                                             htmlOutput("urlTextVertical"), hr(),

                                             h5(tags$b("Directions:")),
                                             helpText(tags$ul(
                                               tags$li("Copy and paste or manually enter market data into the Inputs table."),
                                               tags$li(htmlOutput("directionsVertical")),
                                               tags$li("See the", tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"package vignette for more details about the models used." )
                                               #tags$li("Shares must be between 0 and 1."),
                                               #tags$li("Margins should exclude fixed costs.")
                                             )
                                             ),hr(),
                                             sliderInput("addRowsVertical", "Add rows to Inputs table:", value=10,min=5,max=50,step=5),
                                             # radioButtons("calcElast", "Calibrate model parameters using:",
                                             #              choices = c("market elasticity and 1 or more margins",
                                             #                          "2 or more margins")
                                             # ),
                                             # conditionalPanel(
                                             #   condition = "input.calcElast.includes('elasticity') == true",
                                             #   numericInput("enterElast", "Enter Market Elasticity:", value = -1, min = -Inf, max = 0, step = .1  #, width='75%'
                                             #   )
                                             # ),
                                             # hr(),
                                             #checkboxInput("incEff", "Include Proportional Cost Changes (negative values imply cost reductions)", value = FALSE, width = NULL),
                                             selectInput("mergerTypeVertical", "Merger Type:",
                                                         choices = c("Upstream", "Downstream", "Vertical")),

                                             radioButtons("supplyVertical", "Competitive Interaction:",
                                                          choices = c("Bertrand",
                                                                      "2nd Score Auction")
                                                          ),

                                             ## Use conditionalPanel() to select appropriate demand forms for each unique pair of competitive interaction and margin information
                                             # Bertrand
                                             conditionalPanel(
                                               condition = "input.supplyVertical == 'Bertrand'",
                                               selectInput("demandVertical1", "Downstream Demand Specification:",
                                                           choices = c("logit")),
                                               helpText(tags$b("Note:"), "Share of outside good implied by the sum of inside product shares. Price of outside good fixed at 0.")
                                             ),
                                             # 2nd Score Auction
                                             conditionalPanel(
                                               condition = "input.supplyVertical == '2nd Score Auction'",
                                               selectInput("demandVertical2", "Downstream Demand Specification:",
                                                           choices = c("logit")),
                                               helpText(tags$b("Note:"), "Share of outside good implied by the sum of inside product shares. Price of outside good fixed at 0.")
                                             ),

                                             hr(),
                                             fluidRow(
                                               column(width=12, align = "center",
                                                      tags$div(
                                                        HTML("<font size=\"2\"> Supported by </font>"),
                                                        HTML(logo)
                                                      )
                                               )
                                             )
                                           ),

                                           mainPanel(
                                             h2("Enter Inputs"),
                                              rHandsontableOutput("hotVertical"), br(),
                                             # #tags$head(
                                             #  tags$style(HTML('#run{color:white;background-color:black}'))
                                             #),
                                             actionButton(inputId = "simulateVertical", label = "", icon = icon("play"), width = '60px', style = 'padding:4px')
                                             #)
                                             ,
                                             br(), br(),br(),
                                             tabsetPanel(id = "inTabsetVertical",
                                                         tabPanel("Summary", value = "respanelVertical", br(), br(), tableOutput("resultsVertical"), br(),
                                                                  helpText(tags$b("Note:"), "All price changes as well as compensating marginal cost reduction are (post-merger) share-weighted averages.
                                                                           A negative Consumer Harm number denotes benefit, while a negative Producer Benefit number denotes harm.")
                                                                  ),
                                                         tabPanel("Details", value = "detpanelVertical", br(), tableOutput("results_shareOutVertical"), br(), tableOutput("results_detailedVertical")

                                                                  #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                                                                  #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                                                                  #)
                                                         ),
                                                         tabPanel("Elasticities", value = "elastpanelVertical", br(),
                                                                  radioButtons("pre_elastVertical", "",
                                                                               choices = c("Pre-Merger",
                                                                                           "Post-Merger"
                                                                               ), inline = TRUE),
                                                                  br(),
                                                                  tableOutput("results_mktelastVertical"),
                                                                  tableOutput("results_elastVertical"),
                                                                  conditionalPanel("input.supplyVertical != 'Cournot'",
                                                                                   checkboxInput("diversionsVertical", "Report diversion ratios", value = FALSE),
                                                                                   helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.", "Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                                                                  ),
                                                                  conditionalPanel("input.supplyVertical == 'Cournot'",
                                                                                   helpText(tags$b("Note:"), "above are own-price elasticities")
                                                                  )
                                                         ),
                                                         tabPanel("Diagnostics", value = "diagpanelVertical", br(), h4("Percent Differences between Inputted and Fitted Values Relative to Inputs"), br(),
                                                                  tableOutput("results_diag_elastVertical"),
                                                                  tableOutput("results_diagnosticsVertical"),
                                                                  htmlOutput("overIDTextVertical"),
                                                                  #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                                                                  h4("Parameters"),verbatimTextOutput("parametersVertical"),
                                                                  helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"package vignette for more details about the parameters displayed here." )
                                                         ),
                                                         tabPanel("R Code",  value = "codepanelVertical", br(),verbatimTextOutput("results_codeVertical")),
                                                         tabPanel("Messages", value = "msgpanelVertical", br(), h4("Warnings"), span(textOutput("warningsVertical"), style="color:orange"), br(),
                                                                  h4("Errors"), span(textOutput("errorsVertical"), style="color:red"))

                                                )

                                           )

                                         )
                                       )
                                    ),


                              tabPanel("Documentation",
                                       fluidPage(htmlOutput("referenceATR"))
                              )

                   ),

                   navbarMenu("Numerical Simulations",

                              tabPanel("Horizontal", style = "overflow-y:scroll; max-height: 90vh",
                                       fluidPage(
                                         titlePanel("Horizontal Simulations"),

                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Summary",
                                                      fluidPage(
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            h5(tags$b("Overview:")),
                                                            helpText(tags$ul(
                                                              tags$li(htmlOutput('sumNumMergerATR')),
                                                              tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564982_Using_concentration_measures_for_optimal_screening_of_horizontal_mergers",
                                                                                             "Taragin and Loudermilk (2019)"),"for further details." ))
                                                              )
                                                            ),
                                                            # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                            #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                            #                    selected = "Bertrand ces"),
                                                            selectInput("outcomeSumATR", "Outcomes to Report:",
                                                                        choices = c( "Consumer Harm ($)", "Producer Benefit ($)", "Net Harm ($)","Industry Price Change (%)", "Merging Party Price Change (%)")),
                                                            sliderInput("shareOutSumATR", "Restrict Market by Outside Share (%):", value = 30, min = 10, max = 60, step = 10),  # Ask Charles if we should go from step == 10 to step == 5
                                                            fluidRow(
                                                              column(width=12, align = "center",
                                                                     tags$div(
                                                                       HTML("<font size=\"2\"> Supported by </font>"),
                                                                       HTML(logo)
                                                                     )
                                                              )
                                                            )
                                                            ),
                                                          mainPanel(
                                                            br(),
                                                            fillPage(plotOutput('plotSumATR')),
                                                            wellPanel(h5(tags$b("Description:")),
                                                                      textOutput('capSumATR'))

                                                          )

                                                        )
                                                      )),
                                             tabPanel("Indices",
                                                      fluidPage(
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            h5(tags$b("Overview:")),
                                                            helpText(tags$ul(
                                                              tags$li(htmlOutput('indicNumMergerATR')),
                                                              tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564982_Using_concentration_measures_for_optimal_screening_of_horizontal_mergers",
                                                                                             "Taragin and Loudermilk (2019)"),"for further details." ))
                                                            )
                                                            ),
                                                            # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                            #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                            #                    selected = "Bertrand ces"),
                                                            radioButtons("pooledIndATR", "Plot Display:", choices = c("Pooled", "By Demand Model"), selected = "Pooled"),
                                                            selectInput("indexIndATR", "Index:",
                                                                        choices = c("Firm Count", "HHI", "Delta HHI", "UPP", "CMCR",  "Harm2nd")),
                                                            sliderInput("shareOutIndATR", "Restrict Market by Outside Share (%):", value=30,min=10,max=60,step=10),
                                                            fluidRow(
                                                              column(width=12, align = "center",
                                                                     tags$div(
                                                                       HTML("<font size=\"2\"> Supported by </font>"),
                                                                       HTML(logo)
                                                                     )
                                                              )
                                                            )
                                                          ),
                                                          mainPanel(
                                                            br(),
                                                            fillPage(plotOutput('plotIndATR')),
                                                            wellPanel(h5(tags$b("Description:")),
                                                                      textOutput('capIndATR'))
                                                          )


                                                        )

                                                      )
                                             )), style='width: 100%; height: 100%' #https://stackoverflow.com/questions/19096439/shiny-how-to-adjust-the-width-of-the-tabsetpanel

                                         )

                                    )

                              ),

                              tabPanel("Vertical", style = "overflow-y:scroll; max-height: 90vh",
                                       fluidPage(
                                         titlePanel("Vertical Simulations"),

                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Summary",
                                                      fluidPage(
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            h5(tags$b("Overview:")),
                                                            helpText(tags$ul(
                                                              tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining",
                                                                                             "Sheu and Taragin (2020)"),"for further details."))
                                                            )
                                                            ),
                                                            # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                            #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                            #                    selected = "Bertrand ces"),
                                                            fluidRow(
                                                              column(width=12, align = "center",
                                                                     tags$div(
                                                                       HTML("<font size=\"2\"> Supported by </font>"),
                                                                       HTML(logo)
                                                                     )
                                                              )
                                                            )
                                                          ),
                                                          mainPanel(
                                                            br(),
                                                            fillPage(imageOutput("figSummary", width = "100%", height = "100%")),
                                                            wellPanel(h5(tags$b("Description:")),
                                                                      textOutput('capSummary'))

                                                          )

                                                        )
                                                      )),
                                             tabPanel("Upstream",
                                                      fluidPage(
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            h5(tags$b("Overview:")),
                                                            helpText(tags$ul(
                                                              tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining",
                                                                                             "Sheu and Taragin (2020)"),"for further details."))
                                                            )
                                                            ), hr(),
                                                            # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                            #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                            #                    selected = "Bertrand ces"),
                                                            radioButtons("upstreamPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter"),
                                                            fluidRow(
                                                              column(width=12, align = "center",
                                                                     tags$div(
                                                                       HTML("<font size=\"2\"> Supported by </font>"),
                                                                       HTML(logo)
                                                                     )
                                                              )
                                                            )
                                                          ),
                                                          mainPanel(
                                                            br(),
                                                            fillPage(imageOutput("figUpstream", width = "100%", height = "100%")),
                                                            wellPanel(h5(tags$b("Description:")),
                                                                      textOutput('capUpstream'))
                                                          )


                                                        )

                                                      )
                                             ),
                                           tabPanel("Downstream",
                                                    fluidPage(
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          h5(tags$b("Overview:")),
                                                          helpText(tags$ul(
                                                            tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining",
                                                                                           "Sheu and Taragin (2020)"),"for further details."))
                                                          )
                                                          ), hr(),
                                                          # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                          #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                          #                    selected = "Bertrand ces"),
                                                          radioButtons("downstreamPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter"),
                                                          fluidRow(
                                                            column(width=12, align = "center",
                                                                   tags$div(
                                                                     HTML("<font size=\"2\"> Supported by </font>"),
                                                                     HTML(logo)
                                                                   )
                                                            )
                                                          )
                                                        ),
                                                        mainPanel(
                                                          br(),
                                                          fillPage(imageOutput("figDownstream", width = "100%", height = "100%")),
                                                          wellPanel(h5(tags$b("Description:")),
                                                                    textOutput('capDownstream'))
                                                        )

                                                      )

                                                    )
                                           ),
                                           tabPanel("Vertical",
                                                    fluidPage(
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          h5(tags$b("Overview:")),
                                                          helpText(tags$ul(
                                                            tags$li(helpText("See ",tags$a(href="https://www.researchgate.net/publication/330564874_Simulating_Mergers_in_a_Vertical_Supply_Chain_with_Bargaining",
                                                                                           "Sheu and Taragin (2020)"),"for further details."))
                                                          )
                                                          ), hr(),
                                                          # checkboxGroupInput("supplyModel", label = "Supply Models to Include:",
                                                          #                    choices = list("Bertrand ces", "Bertrand logit", "auction logit"),
                                                          #                    selected = "Bertrand ces"),
                                                          radioButtons("verticalPlot", "Plot Display:", choices = c("By Bargaining Parameter", "By Number of Firms"), selected = "By Bargaining Parameter"),
                                                          fluidRow(
                                                            column(width=12, align = "center",
                                                                   tags$div(
                                                                     HTML("<font size=\"2\"> Supported by </font>"),
                                                                     HTML(logo)
                                                                   )
                                                            )
                                                          )
                                                        ),
                                                        mainPanel(
                                                          br(),
                                                          fillPage(imageOutput("figVertical", width = "100%", height = "100%")),
                                                          wellPanel(h5(tags$b("Description:")),
                                                                    textOutput('capVertical'))
                                                        )

                                                      )

                                                    )
                                           )), style='width: 100%; height: 100%' #https://stackoverflow.com/questions/19096439/shiny-how-to-adjust-the-width-of-the-tabsetpanel
                   )))),

                   navbarMenu("Trade",

                              tabPanel("Tariffs", style = "overflow-y:scroll; max-height: 90vh",

                                       fluidPage(

                                         titlePanel("Simulate a Tariff") ,

                                         sidebarLayout(fluid=TRUE,
                                                       sidebarPanel(

                                                         htmlOutput("urlTextTariffs"),hr(),

                                                         h5(tags$b("Directions:")),
                                                         helpText(tags$ul(
                                                           tags$li("Copy and paste or manually enter market data into the Inputs table."),
                                                           tags$li("Click on the Play button to simulate an", tags$em("ad valorem"), "tariff."),
                                                           tags$li("Default example simulates an increase in the ", tags$em("ad valorem"),"tariff (expressed as a proportion of consumer price) from 5% to 25% on products produced by 'Firm1' and 'Firm2'."),
                                                           tags$li("Products without current or new tariffs are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
                                                           #,tags$li(helpText("See the",tags$a(href=system.file('trade_shiny', package='trade'), "trade"),"package vignette for more details about the models used here." ))
                                                           #tags$li("Shares must be between 0 and 1."),
                                                           #tags$li("Margins should exclude fixed costs.")
                                                         )
                                                         ),hr(),
                                                         sliderInput("addRowsTariffs", "Add rows to Inputs table:", value=10,min=5,max=50,step=5),
                                                         radioButtons("calcElastTariffs", "Calibrate model parameters using:",
                                                                      choices = c("market elasticity AND 1 or more margins",
                                                                                  "2 or more margins")
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.calcElastTariffs.includes('elasticity') == true ",
                                                           numericInput("enterElastTariffs", "Enter Market Elasticity:", value=-1,min=-Inf,max=0,step=.1#, width='75%'
                                                           )
                                                         ),hr(),

                                                         radioButtons("supplyTariffs", "Competitive Interaction:",
                                                                      choices = c("Bertrand",
                                                                                  "Monopolistic Competition",
                                                                                  "Cournot"

                                                                      )),

                                                         # selectInput("demandTariffs", "Demand Specification:",
                                                         #             choices = c("logit", "ces",
                                                         #                         #"linear",
                                                         #                         "aids")),
                                                         ## Use conditionalPanel() to select appropriate demand forms for each unique pair of competitive interaction and margin information
                                                         # Bertrand
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Bertrand' & input.calcElastTariffs.includes('elasticity') == true",
                                                           selectInput("demandTariffs1", "Demand Specification:",
                                                                       choices = c("logit", "ces", "aids"))
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Bertrand' & input.calcElastTariffs.includes('elasticity') == false",
                                                           selectInput("demandTariffs2", "Demand Specification:",
                                                                       choices = c("logit (unknown elasticity)", "ces (unknown elasticity)", "aids (unknown elasticity)"))
                                                         ),
                                                         # Cournot
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Cournot' & input.calcElastTariffs.includes('elasticity') == true",
                                                           selectInput("demandTariffs3", "Demand Specification:",
                                                                       choices = c("linear", "loglinear")),
                                                           helpText(tags$b("Note:"), "Only the first non-missing inputted price and product name is used for Cournot.")
                                                         ),conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Cournot' & input.calcElastTariffs.includes('elasticity') == false",
                                                           selectInput("demandTariffs4", "Demand Specification:",
                                                                       choices = c("linear (unknown elasticity)", "loglinear (unknown elasticity)")),
                                                           helpText(tags$b("Note:"), "Only the first non-missing inputted price and product name is used for Cournot.")
                                                         ),
                                                         # Monopolistic Competition
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Monopolistic Competition' & input.calcElastTariffs.includes('elasticity') == true",
                                                           selectInput("demandTariffs5", "Demand Specification:",
                                                                       choices = c("logit", "ces"))
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Monopolistic Competition' & input.calcElastTariffs.includes('elasticity') == false",
                                                           selectInput("demandTariffs6", "Demand Specification:",
                                                                       choices = c("logit (unknown elasticity)", "ces (unknown elasticity)"))
                                                         ),
                                                         hr(),
                                                         fluidRow(
                                                           column(width=12, align = "center",
                                                                  tags$div(
                                                                    HTML("<font size=\"2\"> Supported by </font>"),
                                                                    HTML(logo)
                                                                  )
                                                           )
                                                         ),
                                                         # conditionalPanel(
                                                         #   condition = "input.supplyTariffs == 'Cournot'",
                                                         #   helpText(tags$b("Note:"), "only the first non-missing inputted price and product name is used for Cournot.")
                                                         # ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == '2nd Score Auction' && input.calcElastTariffs.includes('elasticity') == true",
                                                           helpText(tags$b("Note:"), "2nd score Auction only requires a single price.")
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == '2nd Score Auction' && input.calcElastTariffs.includes('elasticity') == false",
                                                           helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Bertrand' && input.calcElastTariffs.includes('elasticity') == true && input.demandTariffs1 == 'aids'",
                                                           helpText(tags$b("Note:"), "aids does not require pricing information.")
                                                         ),
                                                         conditionalPanel(
                                                           condition = "input.supplyTariffs == 'Bertrand' && input.calcElastTariffs.includes('elasticity') == false && input.demandTariffs2 == 'aids (unknown elasticity)'",
                                                           helpText(tags$b("Note:"), "aids does not require pricing information."))
                                                       ),
                                                       mainPanel(
                                                         h2("Enter Inputs"),
                                                         rHandsontableOutput("hotTariffs"), br(),
                                                         #tags$head(
                                                         #  tags$style(HTML('#run{color:white;background-color:black}'))
                                                         #),
                                                         actionButton(inputId ="simulateTariffs", label = "", icon = icon("play"), width = '60px', style='padding:4px')
                                                         #)
                                                         ,
                                                         br(), br(),br(),
                                                         tabsetPanel(id = "inTabsetTariffs",
                                                                     tabPanel("Summary", value = "respanelTariffs", br(),br(),tableOutput("resultsTariffs"), br(),
                                                                              helpText(tags$b("Note:"), "all price changes are (new tariff) share-weighted averages.
                                                           Negative Consumer Harm or Net Harm numbers denotes benefit.")
                                                                     ),
                                                                     tabPanel("Details", value = "detpanelTariffs", br(),br(), tableOutput("results_shareOutTariffs"),br(), tableOutput("results_detailedTariffs")

                                                                              #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                                                                              #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                                                                              #)
                                                                     ),
                                                                     tabPanel("Elasticities", value = "elastpanelTariffs",  br(),br(),
                                                                              radioButtons("pre_elastTariffs", "",
                                                                                           choices = c("Current Tariff",
                                                                                                       "New Tariff"
                                                                                           ), inline = TRUE),
                                                                              br(),
                                                                              tableOutput("results_mktelastTariffs"),br(),
                                                                              tableOutput("results_elastTariffs"),
                                                                              conditionalPanel("input.supplyTariffs !=='Cournot'",
                                                                                               checkboxInput("diversionsTariffs", "Report diversion ratios", value =FALSE),
                                                                                               helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.","Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                                                                              ),
                                                                              conditionalPanel("input.supplyTariffs == 'Cournot'",
                                                                                               helpText(tags$b("Note:"), "above are own-price elasticities")
                                                                              )
                                                                     ),
                                                                     tabPanel("Diagnostics", value = "diagpanelTariffs", br(),br(), h4("Inputted vs. Fitted Values"),
                                                                              tableOutput("results_diag_elastTariffs"),
                                                                              tableOutput("results_diagnosticsTariffs"),
                                                                              htmlOutput("overIDTextTariffs"),br(),
                                                                              #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                                                                              h4("Parameters"),verbatimTextOutput("parametersTariffs"),
                                                                              helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"),"package vignette for more details about the parameters displayed here." )
                                                                     ),
                                                                     tabPanel("R Code", value = "codepanelTariffs", br(),verbatimTextOutput("results_codeTariffs")),
                                                                     tabPanel("Messages", value = "msgpanelTariffs", br(),h4("Warnings"),  verbatimTextOutput("warningsTariffs"), br(),h4("Errors"),  verbatimTextOutput("errorsTariffs"))

                                                         )

                                                       )

                                         )
                                       )

                              )

                              ,tabPanel("Quotas", style = "overflow-y:scroll; max-height: 90vh",

                                        fluidPage(


                                          titlePanel("Simulate a Quota") ,

                                          sidebarLayout(
                                            sidebarPanel(

                                              htmlOutput("urlTextQuota"),hr(),

                                              h5(tags$b("Directions:")),
                                              helpText(tags$ul(
                                                tags$li("Copy and paste or manually enter market data into the Inputs table."),
                                                tags$li("Click on the Play button to simulate a quota."),
                                                tags$li("Default example simulates an increase in the quota from 100% of current output to 75% of current output on products produced by 'Firm1' and 'Firm2'."),
                                                tags$li("Products without current or new quotas are assumed to be produced domestically. Otherwise, products are assumed to be produced abroad.")
                                                #,tags$li(helpText("See the",tags$a(href=system.file('trade_shiny', package='trade'), "trade"),"package vignette for more details about the models used here." ))
                                                #tags$li("Shares must be between 0 and 1."),
                                                #tags$li("Margins should exclude fixed costs.")
                                              )
                                              ),hr(),
                                              sliderInput("addRowsQuota", "Add rows to Inputs table:", value=10,min=5,max=50,step=5),
                                              radioButtons("calcElastQuota", "Calibrate model parameters using:",
                                                           choices = c("market elasticity and 1 or more margins",
                                                                       "2 or more margins"), selected="market elasticity and 1 or more margins"
                                              ),
                                              conditionalPanel(
                                                condition = "input.calcElastQuota.includes('elasticity') == true ",
                                                numericInput("enterElastQuota", "Enter Market Elasticity:", value=-1,min=-Inf,max=0,step=.1#, width='75%'
                                                )
                                              ),hr(),

                                              radioButtons("supplyQuota", "Competitive Interaction:",
                                                           choices = c("Bertrand"
                                                                       #, "2nd Score Auction"
                                                                       #, "Cournot"
                                                           )),

                                              # selectInput("demandQuota1", "Demand Specification:",
                                              #             choices = c("logit"
                                              #                         #, "ces"
                                              #                         #, "linear"
                                              #                         #, "aids"
                                              #             )),
                                              ## Use conditionalPanel() to select appropriate demand forms for each unique pair of competitive interaction and margin information
                                              # Bertrand
                                              conditionalPanel(
                                                condition = "input.supplyQuota == 'Bertrand' & input.calcElastQuota.includes('elasticity') == true",
                                                selectInput("demandQuota1", "Demand Specification:",
                                                            choices = c("logit"))
                                              ),
                                              conditionalPanel(
                                                condition = "input.supplyQuota == 'Bertrand' & input.calcElastQuota.includes('elasticity') == false",
                                                selectInput("demandQuota2", "Demand Specification:",
                                                            choices = c("logit (unknown elasticity)"))
                                              ),
                                              hr(),
                                              fluidRow(
                                                column(width=12, align = "center",
                                                       tags$div(
                                                         HTML("<font size=\"2\"> Supported by </font>"),
                                                         HTML(logo)
                                                       )
                                                )
                                              ),
                                              conditionalPanel(
                                                condition = "input.supplyQuota == 'Cournot'",
                                                helpText(tags$b("Note:"), "only the first non-missing inputted price and product name is used for Cournot.")
                                              ),
                                              conditionalPanel(
                                                condition = "input.supplyQuota == '2nd Score Auction' && input.calcElastQuota.includes('elasticity') == true",
                                                helpText(tags$b("Note:"), "2nd score Auction only requires a single price.")
                                              ),
                                              conditionalPanel(
                                                condition = "input.supplyQuota == '2nd Score Auction' && input.calcElastQuota.includes('elasticity') == false",
                                                helpText(tags$b("Note:"), "2nd Score Auction does not require prices.")
                                              ),
                                              conditionalPanel(
                                                condition = "input.supplyQuota == 'Bertrand' && input.demandQuota == 'aids'",
                                                helpText(tags$b("Note:"), "aids does not require pricing information.")
                                              )
                                            ),
                                            mainPanel(
                                              h2("Enter Inputs"),
                                              rHandsontableOutput("hotQuota"), br(),
                                              #tags$head(
                                              #  tags$style(HTML('#run{color:white;background-color:black}'))
                                              #),
                                              actionButton(inputId ="simulateQuota" , label = "", icon = icon("play"), width = '60px', style='padding:4px')
                                              #)
                                              ,
                                              br(), br(),br(),
                                              tabsetPanel(id = "inTabsetQuota",
                                                          tabPanel("Summary", value = "respanelQuota", br(),br(),tableOutput("resultsQuota"), br(),
                                                                   helpText(tags$b("Note:"), "all price changes are (new quota) share-weighted averages.
                                                Negative Consumer Harm or Net Harm numbers denotes benefit.")
                                                          ),
                                                          tabPanel("Details", value = "detpanelQuota", br(),br(), tableOutput("results_shareOutQuota"),br(), tableOutput("results_detailedQuota")

                                                                   #,conditionalPanel("input.demand == 'aids' || input.demand == 'ces' || input.demand == 'ces (unknown elasticity)'",
                                                                   #                  helpText(tags$b("Note:"), "shares are revenue-based.")
                                                                   #)
                                                          ),
                                                          tabPanel("Elasticities", value = "elastpanelQuota",  br(),br(),
                                                                   radioButtons("pre_elastQuota", "",
                                                                                choices = c("Current Quota",
                                                                                            "New Quota"
                                                                                ), inline = TRUE),
                                                                   br(),
                                                                   tableOutput("results_mktelastQuota"),br(),
                                                                   tableOutput("results_elastQuota"),
                                                                   conditionalPanel("input.supplyQuota !=='Cournot'",
                                                                                    checkboxInput("diversionsQuota", "Report diversion ratios", value =FALSE),
                                                                                    helpText(tags$b("Note:"), "diagonal elements are own-price elasticities.","Off-diagonal elements are the cross-price elasticities of row with respect to column.")
                                                                   ),
                                                                   conditionalPanel("input.supplyQuota == 'Cournot'",
                                                                                    helpText(tags$b("Note:"), "above are own-price elasticities")
                                                                   )
                                                          ),
                                                          tabPanel("Diagnostics", value = "diagpanelQuota", br(),br(), h4("Inputted vs. Fitted Values"),
                                                                   tableOutput("results_diag_elastQuota"),
                                                                   tableOutput("results_diagnosticsQuota"),
                                                                   htmlOutput("overIDTextQuota"),br(),
                                                                   #helpText(tags$b("Note:"), "Negative numbers mean that observed values are larger than predicted values."),br(),
                                                                   h4("Parameters"),verbatimTextOutput("parametersQuota"),
                                                                   helpText("See the",tags$a(href="https://CRAN.R-project.org/package=antitrust", "antitrust"), "package vignette for more details about the parameters displayed here." )
                                                          ),
                                                          tabPanel("R Code", value = "codepanelQuota", br(),verbatimTextOutput("results_codeQuota")),
                                                          tabPanel("Messages", value = "msgpanelQuota", br(),h4("Warnings"),  verbatimTextOutput("warningsQuota"), br(),h4("Errors"),  verbatimTextOutput("errorsQuota"))

                                              )

                                            )

                                          )
                                        )

                              )
                              ,tabPanel("Documentation",
                                        fluidPage(htmlOutput("referenceTrade"))
                              )
                   )
)


# ,tabPanel("Documentation",
#           tags$iframe(style="height:500px; width:100%; scrolling=yes",
#                       src="https://cran.r-project.org/web/packages/antitrust/vignettes/manual.pdf")
#           )
#
#)
#)
