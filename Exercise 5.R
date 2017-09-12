# Exercise 5

## Manipulating the Data

duration <- faithful$eruptions
waiting <- faithful$waiting

## Linear Regression
eruption_lm = lm(eruptions ~ waiting, data=faithful)
summary(eruption_lm)

fit <- lm(eruption_lm)
fit$coefficients # Coefficients for the LM

fit2 <- summary(lm(eruption_lm))
fit2$r.squared # R-Squared Values for the LM


## Intervals for Predictions

sequence <- data.frame(waiting = seq(min(waiting), max(waiting)))
sequence

conf <- as.data.frame(predict(fit, sequence, interval="confidence")) 
conf


pred <- as.data.frame(predict(fit, sequence, interval="prediction")) 
pred


## Finding the Min and Max of Intervals

min(conf$lwr)
max(conf$upr)


min(pred$lwr)
max(pred$upr)

## Limits

xscale <- c(30,110)
yscale <- c(0, 6.5)

## Correlation

with(faithful, cor(waiting, eruptions))
with(faithful, plot(waiting, eruptions))


## Creating the Graph

plot(waiting, duration, col = "salmon", pch = 16, 
     xlim = xscale, ylim = yscale, 
     xlab = "Waiting", ylab = "Duration", main = "Regression")
abline(fit, col = "purple", lty = 1)
lines(sequence$waiting, conf$lwr, lty = 2, col = "red")
lines(sequence$waiting, conf$upr, lty = 2, col = "red")
lines(sequence$waiting, pred$lwr, lty = 3, col = "skyblue")
lines(sequence$waiting, pred$upr, lty = 3, col = "skyblue")
text(50,5, paste("E =", round(fit$coefficients[2],2),"* W + (", +
                 round(fit$coefficients[1],2),")"))
legend("bottomright", c("Regression", "Confidence", "Prediction"), 
       lty = 1:3, col = c("purple", "red", "skyblue"))
