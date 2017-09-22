---
title: "Exercise 8.R"
output: html_document
runtime: shiny
---
# Exercise 8

## Loading the Packages

library(markdown)
library(knitr)
library(shiny)

## Using the Three Input Arguments

inputPanel(
  sliderInput("n", label = "Support Values:",
              min = 10, max = 1000, value = 100, step = 10),
  sliderInput("mu", label = "Mean:",
              min = -2, max = 2, value = 0, step = 0.1),
  sliderInput("sigma", label = "Standard Deviation:",
              min = 0.2, max = 2, value = 1, step = 0.2)
)

x0 <- -5
xL <- 5
x <- seq(x0,xL,length.out=100)
sigma <- 2

renderPlot({
  y <- dnorm(x, input$mu, sigma)
  plot(x,y, type="l", 
       main = paste0("Normal Distribution X~N(",input$mu,",",sigma,")"),
       ylab="Density",
       xlim = c(x0, xL), 
       ylim = c(0,max(y)))
})