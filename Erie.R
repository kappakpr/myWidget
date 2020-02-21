rm(list=ls())

library(mgcv)
library(TSA)
library(FinTS)
library(ggplot2)
library(forecast)


setwd("C:\\RProgs")

data = read.csv('erie.csv',header=T)
colnames(data) = c('Month', 'Level')
dates = as.Date(data$Month, '%Y-%m')
#level.ts=level.ts[1:599]
data$Level[1:600]
level.ts = ts(data$Level[1:600], start=c(1921,1), frequency = 12)


#Question 1

#A
plot.ts(level.ts, main="Monthly Average Water Levels")


#Quesiton 2

#A
level = data[,2]; level = level[1:600]
H = harmonic(ts(level, start = 1/12, end = 50, deltat = 1/12), 2)
time.pts = c(1:length(level))
time.pts = c(time.pts-min(time.pts))/max(time.pts)
erie.np = gam(level~s(time.pts)+H)
summary(erie.np)

#B
level.dts = diff(level.ts)
length(level.dts)
#par(mfrow=c(1,3))
par(mfrow=c(1,1))
ts.plot(level.dts, main = "Differenced Average Water Levels")
acf(level.dts, main="First Order Difference - ACF")
pacf(level.dts, main="First Order Difference - PACF")

level.dts2 = diff(level.ts, differences=2)
par(mfrow=c(1,3))
ts.plot(level.dts2, main = "Second Order Differences")
acf(level.dts2, main="Second Order Difference - ACF")
pacf(level.dts2, main="Second Order Difference - PACF")

level.dts3 = diff(level.ts, differences=3)
par(mfrow=c(1,3))
ts.plot(level.dts3, main = "Third Order Differences")
acf(level.dts3, main="Third Order Difference - ACF")
pacf(level.dts3, main="Third Order Difference - PACF")

level.dts4 = diff(diff(level.ts, lag=12))
par(mfrow=c(1,3))
ts.plot(level.dts4, main = "Seasonal Differences")
acf(level.dts4, main="Seasonal Difference - ACF",lag=60)
pacf(level.dts4, main="Seasonal Difference - PACF",lag=60)


#Question 3

#A
final.aic = Inf
final.arima.order = c(0,1,0)
final.seasonal.order = c(0,1,0)
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2) {
      for (Q in 0:2) {
        mod = arima(level.ts, order=c(p,1,q), seasonal=list(order=c(P,1,Q), period=12), method="ML")
        current.aic = AIC(mod)
	  print(paste(p,q,P,Q,current.aic,sep="   "))
        if (current.aic < final.aic){
          final.aic = current.aic
          final.arima.order = c(p,1,q)
	    final.seasonal.order = c(P,1,Q)
          final.arima = mod
        }
      }
    }
  }
}

#B
final.arima$coef
confint(final.arima,level=.99)
summary(final.arima)

#C
model.c = arima(level.ts, order=c(1,1,3), seasonal=list(order=c(0,1,1), period=12), method="ML")
AIC(model.c)
BIC(model.c)
AIC(final.arima)
BIC(final.arima)


#Question 4

#A
Box.test(resid(model.c),lag=24,type="Ljung",fitdf=5)
acf(resid(model.c),lag=60)
 
#B
jarque.bera.test(resid(model.c))
par(mfrow=c(1,2))
hist(resid(model.c),breaks="FD",xlab='Standardized Residuals',main='Histogram: Residuals')
qqnorm(resid(model.c))
qqline(resid(model.c))


#Question 5

#A
fit=level.ts-model.c$residuals
mae=mean(abs(fit-level.ts))
pm=sum((fit-level.ts)^2)/sum((level.ts-mean(level.ts))^2)
plot(level.ts, main = "Model 3(c)")
lines(fit, col="red")

fit=level.ts-final.arima$residuals
mae=mean(abs(fit-level.ts))
pm=sum((fit-level.ts)^2)/sum((level.ts-mean(level.ts))^2)
plot(level.ts, main = "Model 3(a)")
lines(fit, col="red")


