library(quantmod)
library(tseries)
library(fGarch)
library(mgcv)
library(rugarch)


gnp = read.csv("GNP.csv",header=T)
head(gnp)
gnp=gnp[,2]
gnpgr=diff(log(gnp))  
ts.plot(gnpgr,ylab="the growth rate of GNP")

n=length(gnpgr)
gnpgr.test = gnpgr[200:n]
gnpgr.train =  gnpgr[-c(200:n)]

## (a) Find the best ARIMA(p,d,q) model 
final.aic = Inf
final.order = c(0,0,0)
for (p in 0:3) for (d in 0:1) for (q in 0:3) {
  current.aic = AIC(arima(gnpgr.train, order=c(p, d, q)))
  if (current.aic < final.aic) {
    final.aic = current.aic
    final.order = c(p, d, q)
    final.arima = arima(gnpgr.train, order=final.order)
  }
}
## What is the selected order?
#> final.order
#[1] 3 0 2

## Residual Analysis
resids = resid(final.arima)
acf(resids,main="Residuals of ARIMA Fit")
acf(resids^2,main="Squared Residuals of ARIMA Fit")


# for serial correlation
Box.test(resids,lag=6,type='Ljung',fitdf=5)
# for heteroscedasticity in residuals (ARCH effect)
Box.test((resids)^2,lag=6,type='Ljung',fitdf=5)


# the low p-value for the former Ljung-Box tests shows that we need to consider ARCH or GARCH model.



## (b) Find the best AR(1) - GARCH (or ARCH) model if necessary
##### Order Selection ################################################
## Find GARCH Order given ARMA order identified before
## ugrach from rugarch libary


final.bic = Inf
final.order = c(0,0)
for (m in 0:3) for (n in 0:3) {
  spec = ugarchspec(variance.model=list(garchOrder=c(m,n)),
                    mean.model=list(armaOrder=c(3, 2), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, gnpgr.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(m, n)
  }
}
#> final.order
#[1] 1 1


## Refine the ARMA order
final.bic = Inf
final.order = c(0,0)
for (p in 0:3) for (q in 0:3) {
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(p, q), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, gnpgr.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(p, q)
  }
}

# > final.order
# [1] 3 0


## Refine the GARCH order
final.bic = Inf
final.order = c(0,0)
for (m in 0:3) for (n in 0:3) {
  spec = ugarchspec(variance.model=list(garchOrder=c(m,n)),
                    mean.model=list(armaOrder=c(3, 0), include.mean=T),
                    distribution.model="std")    
  fit = ugarchfit(spec, gnpgr.train, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  if (current.bic < final.bic) {
    final.bic = current.bic
    final.order = c(m, n)
  }
}

# > final.order
# [1] 1 1


### Goodness of Fit ####################################################
spec.1 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(3, 2), 
                                    include.mean=T), distribution.model="std")    
final.model.1 = ugarchfit(spec.1, gnpgr.train, solver = 'hybrid')

spec.2 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(3, 0), 
                                    include.mean=T), distribution.model="std")    
final.model.2 = ugarchfit(spec.2, gnpgr.train, solver = 'hybrid')



## compare Information Criteria
infocriteria(final.model.1)
infocriteria(final.model.2)

final.model = garchFit(~ arma(3,2)+ garch(1,1), data=gnpgr.train, trace = FALSE)
summary(final.model)
final.model = garchFit(~ arma(3,0)+ garch(1,1), data=gnpgr.train, trace = FALSE)
summary(final.model) 
# All models perform similarly: choose the most reasonable model: the second model



### (c) Prediction ##################################################

## Prediction of the return time series
nfore = length(gnpgr.test)
fore.series = NULL

for(f in 1: nfore){
  ## Fit models
  data = gnpgr.train
  if(f>2)
    data = c(gnpgr.train,gnpgr.test[1:(f-1)])  
  
  final.model = ugarchfit(spec.2, data, solver = 'hybrid')

  ## Forecast
  fore = ugarchforecast(final.model, n.ahead=1)
  fore.series = c(fore.series, fore@forecast$seriesFor)

}

## Compute Accuracy Measures 

### Mean Squared Prediction Error (MSPE)
mean((fore.series - gnpgr.test)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(fore.series - gnpgr.test))
### Precision Measure (PM)
sum((fore.series - gnpgr.test)^2)/sum((gnpgr.test-mean(gnpgr.test))^2)


ts.plot(gnpgr.test)
lines(fore.series,col="blue")

