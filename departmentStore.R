getwd
sales.ts <- ts(read.csv("SouvenirSales.csv"))
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(1500, 85000), bty = "l") 
lines(sales.lm$fitted, lwd = 2)

x=seq(-2,2,0.05)
plot(c(x,x),c(sqrt(1-(abs(x)-1)^2), acos(1-abs(x))-pi
),type='l',lwd=2)
n=100000
U1=runif(n,-2,2)
U2=runif(n,-pi,1)
hitup=which((U2<=sqrt(1-(abs(U1)-1)^2)&U2>=0)==TRUE)
hitlo=which((U2>=acos(1-abs(U1))-pi&U2<=0)==TRUE)



kup=sum((U2<=sqrt(1-(abs(U1)-1)^2)&U2>=0)==TRUE)
klo=sum((U2>=acos(1-abs(U1))-pi&U2<=0)==TRUE)
k=(kup+klo)/100000
area=k*(4*(1+pi))




points(U1[hitup],U2[hitup],col='orange',pch=20)
points(U1[hitlo],U2[hitlo],col='orange',pch=20)


ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2)) 
par(mfrow = c(2, 1)) 
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l") 
lines(ridership.lm$fitted, lwd = 2) 
ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12)) 
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")


