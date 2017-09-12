# Exercise 4

## Changing the Working Directory

setwd('./Documents/UCBX Programming with R/Week 4')

## Loading the Libraries

library(ggplot2)


## Loading the Dataset 

data(diamonds)

## Structure of Diamonds

str(diamonds)


## Writing a Function for Histogram Plots

hist_normal <- function(t, graphStyle ="plot"){ # Creating the function
  if (graphStyle == "histogram") {
    hist(t, probability = TRUE)
  }else if (graphStyle == "density"){
    denst <- density(t)
    plot(denst)
  }else if (graphStyle == "ggplot"){
    hist_plot <- ggplot(as.data.frame(t), aes(t))
    hist_plot + geom_histogram(bins=5, aes(y=..density..)) + geom_density()
  }else if (graphStyle == "qplot"){
    qplot(t, y=..density.., geom=c('histogram', 'density')) 
  }else { #Default plot
    denst <- density(t) 
    histo <- hist(t, freq=FALSE, ylim=c(0, max(denst$y)))
    lines(denst$x, denst$y, col = "darkorange") 
    
  }
}

## Calling the Function on the Original

hist_normal(diamonds$price)
hist_normal(diamonds$price,"plot") 
hist_normal(diamonds$price,"ggplot")
hist_normal(diamonds$price, "qqplot")





