rm(list=ls())
setwd("C:\\RProgs")

popularity = read.csv('popularity.csv')[,c(1,10,6,8,9)]
popularity

dates = as.Date(popularity$Date, '%d-%m-%Y')
approve = ts(popularity$Approving, start=c(2001,3), frequency = 12)
dapprove=diff(log(approve))
dapprove.test = dapprove[-c(1:(length(dapprove)-14))]
dapprove.train = dapprove[c(1:(length(dapprove)-14))]

dapprove.train.ts = ts(dapprove.train, start=c(2001,3), frequency = 12)
#aic

final.aic=Inf
final.order=c(0,0,0)
for (p in 1:5) for (d in 0:1) for (q in 1:5) 
{
  current.aic=AIC(arima(dapprove.train.ts,order=c(p, d, q)))
  #print(paste(p,q,current.aic,sep="   "))
  if(current.aic<final.aic) 
  {
    final.aic=current.aic
    final.order=c(p,d,q)
  }
}

final.order
#2 0 2

final.arima=arima(dapprove.train, order=final.order, method="ML")

resids = resid(final.arima)[-1]
squared.resids=resids^2

par(mfcol=c(1,1))
plot(squared.resids,type='l')

acf(squared.resids)
pacf(squared.resids)

#test for arch effect
Box.test(squared.resids,lag=20,type='Ljung',fitdf=5)
Box.test(squared.resids,lag=20,type='Ljung',fitdf=10)


library(quantmod)
library(tseries)
library(fGarch)
library(mgcv)
library(rugarch)

#GARCH Order Selection
#Select model with smallest BIC (if prediction is the objective)
final.bic = Inf
final.order = c(0,0)
for (p in 1:3) for (q in 1:3)
{
  spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                    mean.model=list(armaOrder=c(1, 0, 1), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, dapprove.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) 
  {
    final.bic = current.bic
    final.order = c(p, q)
  }
}

final.order
#GARCH order [1] 1 1

#Refine the ARMA order
#retain the constraints 4>p , 2>d and 4>q 
final.bic = Inf
final.order.arma = c(0,0)
for (p in 1:3) for (q in 1:3)
{
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(p, q), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, dapprove.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) 
  {
    final.bic = current.bic
    final.order.arma = c(p, q)
  }
} 

final.order.arma
#[1] 1 2
# new order 3 0

#Refine the GARCH order
final.bic = Inf
final.order.garch = c(0,0)
for (p in 1:3) for (q in 1:3)
{
  spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                    mean.model=list(armaOrder=c(1, 2), 
                                    include.mean=T), distribution.model="std")    
  fit = ugarchfit(spec, dapprove.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) 
  {
    final.bic = current.bic
    final.order.garch = c(p, q)
  }
} 

final.order.garch
#[1] refined GARCH order : 1 1

spec1 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1, 0,2), 
                                  include.mean=T), distribution.model="std")  
final.model1 = ugarchfit(spec1, dapprove.train, solver = 'hybrid')


spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(0, 1,0), 
                                    include.mean=T), distribution.model="std")  
final.model = ugarchfit(spec, dapprove.train, solver = 'hybrid')
final.model



#Residual Analysis 
resids.final.model = residuals(final.model)
par(mfcol=c(2,1))
acf(resids.final.model)
acf(resids.final.model^2)                    
Box.test(resids.final.model,lag=10,type='Ljung')
Box.test(resids.final.model^2,lag=10,type='Ljung')
qqnorm(resids.final.model)
qqnorm(resids.final.model^2)



nfore = length(dapprove.test)
fore.series = NULL
fore.sigma= NULL

for(f in 1: nfore)
{
  #Fit models
  data = dapprove.train
  if(f>2)
    data = c(dapprove.train,dapprove.test[1:(f-1)])  
    final.model = ugarchfit(spec, data, solver = 'hybrid')    

  #Forecast
  fore = ugarchforecast(final.model, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)
  fore.sigma = c(fore.sigma, fore@forecast$sigmaFor)
  
}
fore.series[is.nan(fore.series)]=0
fore.series

#Compute Accuracy Measures 
#Mean Squared Prediction Error (MSPE)
mean((fore.series - dapprove.test)^2)

#Mean Absolute Prediction Error (MAE)
mean(abs(fore.series - dapprove.test))

#Mean Absolute Percentage Error (MAPE)
mean(abs(fore.series - dapprove.test)/(dapprove.test+0.000001))

#Precision Measure (PM)
sum((fore.series - dapprove.test)^2)/sum((dapprove.test-mean(dapprove.test))^2)


ts.plot(dapprove.test)
lines(fore.series,col="blue")

par(mfcol=c(1,1))
var=fore.series - dapprove.test
ts.plot(var)
lines(dapprove.test^2,col="blue")

