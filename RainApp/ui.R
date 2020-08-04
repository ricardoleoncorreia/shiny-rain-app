library(shiny)

shinyUI(fluidPage(
    titlePanel("Random Forest Grid Search Simulator"),
    p("Changing hyper-parameters values, you can see how well a Random Forest model fits the data. Different configurations implies different decision boundaries!"),
    p('After clicking on "Train model" button, model metrics will appear below the plot. Instructions to use this application are below the configuration panel.'),
    sidebarLayout(
        sidebarPanel(
            h4("Configuration panel"),
            sliderInput("num.trees", "Number of trees:", min = 1, max = 30, value = 15),
            sliderInput("max.depth", "Max tree depth:", min = 1, max = 20, value = 10),
            actionButton("button", "Train model"),
            br(),
            br(),
            p("Instructions:"),
            p("1. Select number of trees."),
            p("2. Select maximum tree depth."),
            p('3. Click on "Train model".'),
            p("4. Enjoy!"),
            br(),
            br(),
            p("Note:"),
            p('For confusion matrix, each column represents the real label. The first row are "No" predictions and second row "Yes" predictions.')
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
