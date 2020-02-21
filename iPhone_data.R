rm(list=ls())
setwd("C:\\RProgs")

library(TSA) 
library(data.table) 
data=read.csv("iPhone_data.csv",header=T)

## Process Dates
year = data$Year
class(year)
month = data$Quarter
day = as.list(rep(15, 38))
datemat = cbind(as.character(day),as.character(month),as.character(year))
paste.dates = function(date){
  day = date[1]; month=date[2]; year = date[3]
  return(paste(day,month,year,sep="/"))
}
dates = apply(datemat,1,paste.dates)
dates = as.Date(dates, format="%d/%m/%Y")
data = cbind(dates,data)

temp=ts(data[,"iPhone_sales"],start=2008, freq=4)

attach(data)

#sales.tr = log(iPhone_sales)
#ts.sales = ts(rev(sales.tr),start=2008,frequency=4)

plot(temp,main="iphone data")
#acf(temp,main="iphone data acf")
#pacf(temp,main="iphone data pacf")

#difference first order
xts = diff(temp, lag=1,differences = 1)
ts.plot(xts,ylab="iphone data Differenced")
#acf(xts,main="iphone data Differenced acf")
#pacf(xts,main="iphone data Differenced pacf")

#log transform
temp.log = log(temp)
plot(temp.log,main="log iphone data")
acf(temp.log,main="log iphone data acf")
pacf(temp.log,main="log iphone data pacf")


ts.temp = ts(rev(temp.log),start=2008,frequency=4)
## Differencing to Remove Trend
diff.ts.temp = diff(ts.temp)
plot(diff.ts.temp,main="one diff")
acf(as.vector(diff.ts.temp),main="ACF: One-Lag Difference Iphone sales")
pacf(diff.ts.temp,main="PACF: One-Lag Difference Iphone sales")

## Apply ARIMA

## Order selection -- AIC 
n = length(ts.temp)
norder = 6
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
  for(j in 1:norder){
    modij = arima(ts.temp,order = c(p[i],1,q[j]), method='ML')
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }  
}

aicv = as.vector(aic)  
plot(aicv,ylab="AIC values")
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1
qorder = indexq[indexaic]-1

final_model = arima(ts.temp, order = c(porder,1,qorder), method = "ML")


## GOF: residual analysis
par(mfrow=c(2,2))
plot(resid(final_model), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(final_model),main="ACF: Residuals")
hist(resid(final_model),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(final_model),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(final_model))

Box.test(final_model$resid, lag = (porder+qorder+1), type = "Box-Pierce", fitdf = (porder+qorder))
Box.test(final_model$resid, lag = (porder+qorder+1), type = "Ljung-Box", fitdf = (porder+qorder))

## Forecasting with ARIMA 
## 6 last point: 
n = length(ts.temp)
nfit = n-6
outprice = arima(ts.temp[1:nfit], order = c(porder,1,qorder),method = "ML")
outpred = predict(outprice,n.ahead=6)
ubound = outpred$pred+1.96*outpred$se
lbound = outpred$pred-1.96*outpred$se
ymin = min(exp(lbound))
ymax = max(exp(ubound))
#plot(rev(temp)[(n-38):n],exp(ts.temp[(n-38):n]),type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Iphone sales")
plot(data$dates,ts.temp,type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Iphone sales")
points(data$dates[(nfit+1):n],exp(outpred$pred),col="red")
lines(data$dates[(nfit+1):n],exp(ubound),lty=3,lwd= 2, col="blue")
lines(data$dates[(nfit+1):n],exp(lbound),lty=3,lwd= 2, col="blue")

