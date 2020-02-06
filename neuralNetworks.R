setwd("C:/Users/USER/Documents/BA")

library(forecast)
library(zoo)

fortified <- read.csv("AustralianWines.csv")

#### Set training and validation periods

fortified.ts <- ts(fortified$Fortified, start = c(1,1), end = c(14,12), frequency = 12) 
nValid <- 12
nTrain = length(fortified.ts) - nValid

train.fortified.ts <- window(fortified.ts, start = c(1,1) , end = c(1, nTrain))
valid.fortified.ts <- window(fortified.ts, start = c(1,nTrain+1), end = c(1, nTrain + nValid))

#### projection with neural network
 
fortified.nnetar <- nnetar(train.fortified.ts, repeats = 20, p = 11, P = 1, size = 7)
summary(fortified.nnetar$model[[1]]) 
fortified.nnetar.pred <- forecast(fortified.nnetar, h = nValid)
accuracy(fortified.nnetar.pred, valid.fortified.ts)
plot(train.fortified.ts, main= "Fortified Wine Sales", ylab = "Sales", xlab = "Time", bty = "l", lty = 1) 
#axis(1,at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(fortified.nnetar.pred$fitted, lwd = 2, col = "blue") 
#lines(fortified.nnetar.pred$mean, lwd = 2, col = "blue", lty = 2) 
#lines(valid.fortified.ts)

###### residuals
residuals <- fortified.nnetar.pred$residuals 
plot(residuals, main= "Residuals", ylab = "Error", xlab = "Time", bty = "l", lty = 1)


#### ETS

fortified.ets <- ets(train.fortified.ts, model = "ZZZ")
fortified.ets.pred <- forecast(fortified.ets, h = nValid, level = 0)
summary(fortified.ets)