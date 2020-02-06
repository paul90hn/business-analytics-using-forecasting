setwd("C:/Users/USER/Documents/BA/r")

library(forecast)
library(zoo)

sept11Travel <- read.csv("Sept11Travel.CSV")

air.ts <- ts(sept11Travel$Air, start = c(1990,1), end = c(2004,4), frequency = 12)
rail.ts <- ts(sept11Travel$Rail, start = c(1990,1), end = c(2004,4), frequency = 12)
vehicle.ts <- ts(sept11Travel$Vehicle, start = c(1990,1), end = c(2004,4), frequency = 12)
attack.ts <- ts(sept11Travel$Intervention, start = c(1990,1), end = c(2004,4), frequency = 12)
#season.ts <- ts(sept11Travel$Season, start = c(1990,1), end = c(2004,4))

nValid <- 16
nTrain <- length(air.ts) - nValid

train.air.ts <- window(air.ts, start = c(1990,1), end = c(1990,nTrain))
valid.air.ts <- window(air.ts, start = c(1990,nTrain + 1), end = c(1990,nValid + nTrain))
train.rail.ts <- window(rail.ts, start = c(1990,1), end = c(1990,nTrain))
valid.rail.ts <- window(rail.ts, start = c(1990,nTrain + 1), end = c(1990,nValid + nTrain))
train.vehicle.ts <- window(vehicle.ts, start = c(1990,1), end = c(1990,nTrain))
valid.vehicle.ts <- window(vehicle.ts, start = c(1990,nTrain + 1), end = c(1990,nValid + nTrain))
train.attack.ts <- window(attack.ts, start = c(1990, 1), end = c(1990,nTrain))


#valid.attack.ts <- window(attack.ts, start = c(1990, nTrain + 1), end = c(1990, nValid + nTrain))


train.air.lm <- tslm(train.air.ts ~ trend + season + train.attack.ts) 
train.rail.lm <- tslm(train.rail.ts ~ trend + season + train.attack.ts) 
train.vehicle.lm <- tslm(train.vehicle.ts ~ trend + season + train.attack.ts) 

summary(train.air.lm)
summary(train.rail.lm)
summary(train.vehicle.lm)

train.air.lm.pred <- forecast(train.air.lm, h = nValid, level = 0)
   



plot(train.air.lm.pred, main = "", bty = "l", flty = 2)
lines(train.air.lm.pred$fitted, lwd = 2, col = "blue") 
lines(valid.air.ts)



