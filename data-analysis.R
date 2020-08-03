library(tidyr); library(dplyr); library(ggplot2); library(caret); library(randomForest);

num.trees <- 10
max.depth <- 70

weather.data <- read.csv('RainApp/weatherAUS.csv')

ggplot(weather.data, aes(x=MaxTemp, y=Humidity3pm, color=RainTomorrow)) +
  geom_point()

trainIndex <- createDataPartition(weather.data$RainTomorrow, p=0.80, list=FALSE)

train <- weather.data[trainIndex,]
test <- weather.data[-trainIndex,]

rf.fit <- ranger(RainTomorrow ~ .,
                 data = train,
                 num.trees = num.trees,
                 write.forest = TRUE,
                 max.depth = max.depth,
                 seed = 42)

train.pred <- predict(rf.fit, data = train)$predictions
test.pred <- predict(rf.fit, data = test)$predictions

train.accuracy <- confusionMatrix(train.pred, train$RainTomorrow)$overall['Accuracy']
test.accuracy <- confusionMatrix(test.pred, test$RainTomorrow)$overall['Accuracy']

c(train.accuracy, test.accuracy)