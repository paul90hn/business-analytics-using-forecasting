getwd

souvenirSales <- read.csv("SouvenirSales.CSV")
sales.ts <- ts(souvenirSales$Sales, start = c(1995,1), end = c(2001,12), frequency = 12)
nValid <- 12
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(1995,1), end = c(1995, nTrain))
valid.ts <- window(sales.ts, start = c(1995, nTrain +1), end = c(1995, nTrain + nValid))

train.lm.season <- tslm(train.ts ~ trend + season) 
summary(train.lm.season)

train.lm.season.pred <- forecast(train.lm.season, h = nValid, level = 0)
plot(train.lm.season.pred, main = "Regression Model with seasonality", ylim = c(1500,85000), ylab = "Sales", xlab = "Time", bty = "l", flty = 2)
lines(train.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)



##### 2222222222  forecast with log sales #####

train.exp.lm.season.ts <- tslm(train.ts ~ trend + season , lambda = 0) 
train.exp.lm.season.pred <- forecast(train.exp.lm.season.ts, h = nValid, level = 0)
summary(train.exp.lm.season.ts)



plot(train.exp.lm.season.pred, main = "Regression Model with seasonality and exp trend", ylim = c(1500,120000), ylab = "Sales", xlab = "Time", bty = "l", flty = 2)
lines(train.exp.lm.season.pred$fitted, lwd = 2, col = "blue") 
lines(valid.ts)





