# Exercise 7

## Loading the Libraries

library(MASS)
library(caret)  # The main package 
library(e1071)  # A helper package caret depends on
library(randomForest)  # A helper package caret depends on

## Creating the Vectors

training_percent <- c(0.3,0.5,0.7,0.9)
accur <- c()
iter <- c()
data <- data.frame()

## For Loop Function

for (j in 1:50) { # 50 iterations
  for (i in 1 : length(training_percent)){
    trainid <- createDataPartition(iris$Species, p=training_percent[i], list=FALSE)
    training <- iris[trainid,] 
    testing <- iris[-trainid,] 
    lda_fit <- train(Species~., data=training, method="lda")
    pred <- predict(lda_fit, testing)
    cm <- confusionMatrix(pred, testing$Species)
    accur[i] <- cm$overall[1]
    iter[i] <- i
  }
  data <- rbind(data,data.frame(accur,iter,j))
}


## Boxplot

boxplot(accur~iter,data, main = "LDA Accuracy Analysis for Iris Species", col = "green")
