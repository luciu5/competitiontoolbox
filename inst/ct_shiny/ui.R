
#The dependency should be listed in NAMESPACE, making this library(shiny) argument redundant.
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("CT Shiny Skeleton"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),


    # Addition of Vanderiblt logo at the bottom of interface (insert vandy.png in www folder to display image)
    # fluidRow(
    #     column(width=12, align = "center",
    #            tags$div(
    #                HTML("<font size=\"2\"> Supported by </font>"),
    #                tags$a(href="https://www.vanderbilt.edu/",
    #                       tags$img(src="vandy.png",alt="Vanderbilt University",style="height:50px"))
    #            )
    #     )
    # )


))
