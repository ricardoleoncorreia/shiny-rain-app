library(shiny); library(ggplot2); library(ranger); library(caret);

shinyServer(function(input, output) {
    set.seed(2020-08-02)
    weather.data <- read.csv('weatherAUS.csv')
    
    trainIndex <- createDataPartition(weather.data$RainTomorrow, p=0.80, list=FALSE)

    train <- weather.data[trainIndex,]
    test <- weather.data[-trainIndex,]

    accuracies <- eventReactive(input$button, {
                        rf.fit <- ranger(RainTomorrow ~ .,
                                         data = train,
                                         num.trees = input$num.trees,
                                         write.forest = TRUE,
                                         max.depth = input$max.depth)
                        
                        train.pred <- predict(rf.fit, data = train)$predictions
                        test.pred <- predict(rf.fit, data = test)$predictions
                        
                        train.accuracy <- confusionMatrix(train.pred, train$RainTomorrow)$overall['Accuracy']
                        
                        train.cm <- confusionMatrix(train.pred, train$RainTomorrow)
                        train.cm.as.vector <- as.vector(train.cm)$table
                        train.accuracy <- train.cm$overall['Accuracy']

                        test.cm <- confusionMatrix(test.pred, test$RainTomorrow)
                        test.cm.as.vector <- as.vector(test.cm)$table
                        test.accuracy <- test.cm$overall['Accuracy']
                        
                        c(train.cm.as.vector, train.accuracy, test.cm.as.vector, test.accuracy)
    })
    
    output$dataPlot <- renderPlot({
        ggplot(weather.data, aes(x=MaxTemp, y=Humidity3pm, color=RainTomorrow)) +
            geom_point() +
            xlab("Max Temp (celsius)") +
            ylab("Humidity 3pm (percent)")
    })

    # =========== Train Results ===========

    output$trainTitle <- renderText({
        accuracies()
        "Training set results"
    })
    
    output$trainText <- renderText({
        accuracy <- round(100 * accuracies()[5], 4)
        paste("Accuracy: ", accuracy, "%")
    })
    
    output$cm.train <- renderText({
        accuracies()
        "Confusion Matrix:"
    })
    
    output$confusionMatrix.train <- renderTable({
        cm.values <-accuracies()[1:4]
        reference.no <- cm.values[1:2]
        reference.yes <- cm.values[3:4]
        test.cm <- cbind(reference.no, reference.yes)
        colnames(test.cm) <- c("No", "Yes")
        test.cm
    })
    
    # =========== Test Results ===========
    
    output$testTitle <- renderText({
        accuracies()
        "Testing set results"
    })

    output$testText <- renderText({
        accuracy <- round(100 * accuracies()[10], 4)
        paste("Test: ", accuracy, "%")
    })
    
    output$cm.test <- renderText({
        accuracies()
        "Confusion Matrix:"
    })
    
    output$confusionMatrix.test <- renderTable({
        cm.values <-accuracies()[6:9]
        reference.no <- cm.values[1:2]
        reference.yes <- cm.values[3:4]
        test.cm <- cbind(reference.no, reference.yes)
        colnames(test.cm) <- c("No", "Yes")
        test.cm
    })
})
