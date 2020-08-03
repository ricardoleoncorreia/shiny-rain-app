library(shiny)

shinyUI(fluidPage(
    titlePanel("Random Forest Grid Search Simulator"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("num.trees", "Number of trees:", min = 1, max = 30, value = 15),
            sliderInput("max.depth", "Max tree depth:", min = 1, max = 20, value = 10),
            actionButton("button", "Train model")
        ),
        mainPanel(
            h4("Weather conditions"),
            plotOutput(outputId = "dataPlot"),
            fluidRow(
                column(6,
                       textOutput("trainTitle"),
                       textOutput("trainText"),
                       br(),
                       textOutput("cm.train"),
                       tableOutput("confusionMatrix.train")
                ),
                column(6,
                       textOutput("testTitle"),
                       textOutput("testText"),
                       br(),
                       textOutput("cm.test"),
                       tableOutput("confusionMatrix.test")
                )
            ),
        )
    )
))
