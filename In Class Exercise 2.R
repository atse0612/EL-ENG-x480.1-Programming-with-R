# In-Class Exercise 2

## Loading the Library
library(RCurl)

## Getting the Dataset
rawElec <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/forecast-methodology/historical-senate-predictions.csv")
dfElec <- read.csv(text = rawElec)


## Replacing the "Lose" Level for the "Loss"

levls <- factor(dfElec$result)
levels(levls) <- c("Loss", "Loss", "Win")
dfElec$result <- levls

## Subsetting to 2012
dfElec2012 <- subset(dfElec, year == 2012)

## Subsetting Win and Loss

Win <- subset(dfElec2012, result == "Win")
Loss <- subset(dfElec2012, result == "Loss")

## Creating a Boxplot

boxplot(Loss$forecast_prob, Win$forecast_prob,
                 names = c("Loss", "Win"),
                 main = "Forecast Probabilities for the 2012 Election Year",
                 xlab = "Results",
                 ylab = "Forecast Probability Random Variable Values")


## Saving the Boxplot

png("forecast_prob.png")


## Saving the Modified Data Frame to R Data File

save(dfElec, file="dfElec.rData")