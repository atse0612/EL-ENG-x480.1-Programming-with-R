# Exercise 6

library(MASS)

## Manipulating the Data

duration <- faithful$eruptions
waiting <- faithful$waiting

## Linear Regression
eruption_lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption_lm)

## Creating the Residuals Variable

resid <- eruption_lm$residuals

## Creating the Histograms
par(mfrow = c(1,2))
hist(rnorm(resid), col = "salmon")
hist(rnorm(resid), col = "magenta", freq = FALSE)

## Creating the QQplots

qqnorm(resid, col = "red")
qqline(resid, col = "blue")

### The graphs for both the histogram and qqplots are both normally distributed, and on the same distribution.


## Sample Mean Calculation

rx <- rnorm(resid)  
fitdistr(rx, "normal")   
fit <- fitdistr(rx, "normal")  
fit$estimate

### Sample mean is not effectively zero. Although, it is below zero.

## Shapiro-Wilk Test for Normality

normrnd <- rnorm(resid)
shapiro.test(normrnd)

### For the normally distributed population, it shows that the p-value is > 0.05, 
### so we can fail to reject the null hypothesis, and true mean is zero. Test statistic is accurate.


## T-Test

t1 <- t.test(normrnd)
t1$p.value

t2 <- rnorm(normrnd)
t.test(normrnd, t2)

### Fail to reject the null hypothesis as it is > 0.05. True mean is zero.

## The regression analysis from last week does not meet the distributional assumptions 
## for confidence intervals because we fail to reject the null hypothesis.




