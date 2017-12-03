# Question 1

## Loading the Libraries
library(ggplot2)
library(dplyr)

## Setting the Working Directory

setwd('./Documents/UCBX Programming with R/Last')

## Reading the Dataset and Subsetting

insom <- read.csv("./atussum_2016.dat")
insom <- subset(insom, select = c(TESEX, t010102))

## Performing the For Loop by Having Empty Data Frame and Time Increments

timex <- seq(10,120, by=0.1)
chp <- data.frame()

for (t in timex){
  insom$insomnia <- insom$t010102 >= t
  chsq <- chisq.test(insom$TESEX, insom$insomnia)
  chp <- rbind(chp, c(t, chsq$p.value))
}


## Getting the tot and pv Columns
names(chp) <- c('tol', 'pv')


## Significance for < 0.05

chp$significant <- chp$pv < 0.05 

## Visualization

ggplot(chp, aes(x = tol, y = pv, col = significant)) + 
  geom_line(size=1, data = chp[-chp$significant, ]) + 
  geom_line(size=1, data = chp[chp$significant,]) +
  labs(title="Gender Disparity for Insomnia Depending on Definition", 
       x="Insomnia Definition Minimum (minutes)",
       y="Chi-Square Independence Test P-Value",
       caption = "Source: Bureau of Labor Statistics' American Time Use Survey 2016",
       legend="Significant?") +
  scale_color_discrete(name="Significant", 
                       labels=c("No", "Yes"))
