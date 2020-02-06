

setwd("C:/Users/USER/Documents/BA/r")

library(forecast)
library(zoo)
library(plotly)

souvenirSales <- read.csv("SouvenirSales.CSV")
sales.ts <- ts(souvenirSales$Sales, start = c(1995,1), end = c(2001,12), frequency = 12)
nValid <- 12
nTrain <- length(sales.ts) - nValid

train.ts <- window(sales.ts, start = c(1995,1), end = c(1995, nTrain))
valid.ts <- window(sales.ts, start = c(1995, nTrain +1), end = c(1995, nTrain + nValid))

#forecast February 2002

train.exp.lm.season.ts <- tslm(train.ts ~ trend + season, lambda = 0) 
train.exp.lm.season.pred <- forecast(train.exp.lm.season.ts, h = nValid + 2, level = 0)


summary(train.exp.lm.season.ts)

accuracy(train.exp.lm.season.pred)  #To test statistical accuracy

ff<-forecast(train.exp.lm.season.ts, h = nValid +2 , level = 0)

plot(train.exp.lm.season.pred, main = "Regression Model with seasonality and exp trend", ylim = c(1500,120000), ylab = "Sales", xlab = "Time", bty = "l", flty = 2)
lines(train.exp.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)


#forecasttable <- write.table(train.exp.lm.season.pred, file = "AR1", col.names = TRUE)

Acf(train.ts, lag.max = 15, main ="") #to compute autocorrelation at different lags

#B) three steps:
#     1) Forecast TS (Done before)
#     2) Forecast errors
#     3) Add error to forcast



# 2) forecast Errors
train.error.lag1.ar <- arima(train.exp.lm.season.pred$residuals, order = c(2,0,0))
train.error.lag1.ar.pred <- forecast(train.error.lag1.ar, h = nValid + 2)
#errors <- train.exp.lm.season.pred$residuals

errors.table <- write.table(train.error.lag1.ar.pred, file = "errorsForecast.txt", sep = " ")
summary(train.error.lag1.ar)
#par(mfrow = c(2, 1))
#plot(ff, main = "Regression Model with seasonality and exp trend", ylim = c(1500,120000), ylab = "Sales", xlab = "Time", bty = "l", flty = 2)
#lines(train.exp.lm.season.pred$fitted, lwd = 2, col = "blue") 
#lines(valid.ts)
plot(train.exp.lm.season.ts$residuals, main = "Errors", ylab = "Errors", xlab = "Time", bty = "l", flty = 2)
lines(train.error.lag1.ar.pred$fitted, lwd = 2, col = "blue")

