
setwd("C:/Users/USER/Documents/BA/r")

library(forecast)
library(zoo)

australianFortified <- read.csv("AustralianWines.CSV")
sales.ts <- ts(australianFortified$Fortified, start = c(2001,1), end = c(2015,12), frequency = 12)
nValid <- 12
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(2001,1), end = c(2001, nTrain))
valid.ts <- window(sales.ts, start = c(2001, nTrain +1), end = c(2001, nTrain + nValid))


train.lm.ts <- tslm(train.ts ~ trend + season , lambda = 0) 
forecast(train.lm.ts, h = nValid, level = 0)
summary(train.lm.ts)

plot(train.lm.pred, main = "Fortified Wine Sales", ylim = c(1000,6000), ylab = "Sales", xlab = "Time", bty = "l", flty = 2)
lines(train.lm.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)



residuals.ts <- train.lm.ts$residuals
plot(residuals.ts, main = "Residuals", bty = "l", flty = 2)

ff<-forecast(train.lm.ts, h = nValid + 2, level = 0)
plot(ff, main = "", bty = "l", flty = 2)

