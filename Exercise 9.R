# Exercise 9

## Loading the Libraries

library(XML)
library(plyr)
library(reshape)
library(ggplot2)
library(RColorBrewer)

## Setting the Working Directory

setwd('./Documents/UCBX Programming with R/Week 9')

## Reading the XML File

caDrought <- xmlToList("./state-CA.xml")

caDroughtFin <- data.frame()
for (i in 1 : (length(caDrought)-1)){
  drought <- data.frame(Date = caDrought[[i]]$.attrs[2], D0 = as.numeric(caDrought[[i]]$D0), D1 = as.numeric(caDrought[[i]]$D1), 
                        D2 = as.numeric(caDrought[[i]]$D2), D3 = as.numeric(caDrought[[i]]$D3), D4 = as.numeric(caDrought[[i]]$D4), Nothing = as.numeric(caDrought[[i]]$Nothing))
  caDroughtFin <- rbind(caDroughtFin, drought)}

str(caDroughtFin)

## Computing the Equations

caDroughtFin$D3 <- caDroughtFin$D3 - caDroughtFin$D4
caDroughtFin$D2 <- caDroughtFin$D2 - (caDroughtFin$D3 + caDroughtFin$D4)
caDroughtFin$D1 <- caDroughtFin$D1 - (caDroughtFin$D2 + caDroughtFin$D3 + caDroughtFin$D4)
caDroughtFin$D0 <- caDroughtFin$D0 - (caDroughtFin$D1 + caDroughtFin$D2 + caDroughtFin$D3 + caDroughtFin$D4)

## Reshaping and Melting the Data

caDrought_forplot <- melt(caDroughtFin, id=c('Date'))
caDrought_forplot$Date <- as.Date(caDrought_forplot$Date, format = "%m/%d/%Y")
names(caDrought_forplot) <- c("Date","Status","Percent")
subset(caDrought_forplot, caDrought_forplot$Date == '05/03/2016')

## Creating the Legend

levels(caDrought_forplot$Status) <- c("No Drought","Abnormally Dry","Moderate Drought","Severe Drought","Extreme Drought","Exceptional Drought")

## Creating the Plot

heater <- ggplot(caDrought_forplot, aes(x = Date, y = Percent, fill = Status))
heater <- heater + geom_area() + 
  ggtitle("Percentage of California Drought WY2013 - Present") + 
  scale_fill_brewer(palette = "YlOrRd")
heater
