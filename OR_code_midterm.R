setwd("C:/Users/pharati3/Documents/Classes/6402/new/midterm")

library(data.table)
library(TSA)
library(mgcv)

#Load data
data=read.csv("HouseData.csv",header=TRUE)
#Split data into training and testing
data.train=data[1:(nrow(data)-4),]
data.test=data[(nrow(data)-3):nrow(data),]

#Original time series
HFS=data.train$Houses_for_Sale
HFS.ts=ts(HFS, frequency = 4, start = 1976)
ts.plot(HFS.ts, ylab = "Houses for Sale", xlab = "Year", main = "Original Time Series of Homes for Sale Over Time")

#Transformed series
HFS.dts= diff(HFS.ts)
ts.plot(HFS.dts, ylab = "Houses for Sale", xlab = "Year", main = "Differenced Time Series of Homes for Sale Over Time")


#ARIMA
final.aic = Inf
final.order = c(0,0,0)
for (p in 0:5) for (q in 0:5) {
	current.aic=AIC(arima(HFS.ts, order=c(p,1,q),method="ML"))
	if (current.aic < final.aic) {
		final.aic = current.aic
		final.order = c(p,1,q)
		final.arima=arima(HFS.ts, order=final.order)
	}
}
par(mfcol=c(1,2))
acf(resid(final.arima),main="ACF: Residuals")
pacf(resid(final.arima),main="PACF: Residuals")
Box.test(final.arima$resid, lag = sum(final.order), type = "Ljung-Box", fitdf = (sum(final.order)-1))

#VAR
library(vars)
ddata.train=apply(data.train[,3:7],2,function(x){diff(ts(x,frequency=4,start=1976))})
model.var=VAR(ddata.train, p=2)
summary(model.var)
## Residual Analysis: Constant Variance Assumption
arch.test(model.var)
## Residual Analysis: Normality Assumption (Jacques Bera Test)
normality.test(model.var)
## Residual Analysis: Uncorrelated Errors Assumption
serial.test(model.var)
model.var.restrict=restrict(model.var)
model.var.restrict

#Predictions
##ARIMA
arima.pred = as.numeric(predict(final.arima,n.ahead=4)$pred)
##VAR
mvar.predict = predict(model.var.restrict, n.ahead=4)
mvar.pred.dhfs = mvar.predict[[1]]$Houses_for_Sale[,1]
mvar.pred.hfs = rep(0,4)
mvar.pred.hfs[1] = HFS.ts[length(HFS.ts)] + mvar.pred.dhfs[1]
mvar.pred.hfs[2] = mvar.pred.hfs[1] + mvar.pred.dhfs[2]
mvar.pred.hfs[3] = mvar.pred.hfs[2] + mvar.pred.dhfs[3]
mvar.pred.hfs[4] = mvar.pred.hfs[3] + mvar.pred.dhfs[4]
#Alternatively
mvar.pred.hfs=diffinv(mvar.pred.dhfs,xi=data.train[164,3])[-1]
##Plot
HFS.all=ts(data$Houses_for_Sale, frequency = 4, start = 1976)
plot(time(HFS.all)[160:168],data$Houses_for_Sale[160:168], type="l", xlab="Year", ylab="Housing For Sale", main="Housing for Sale")
lines(time(HFS.all)[165:168],arima.pred,col="blue",lwd=2)
lines(time(HFS.all)[165:168],mvar.pred.hfs,col="green",lwd=2)
d=par("usr") #grabs the plotting region dimensions
legend(d[1]+.5,d[4]-10,legend=c("ARIMA","VAR"),col=c("blue","green"),lty=1)
##PM
obs = data.test$Houses_for_Sale
sum((arima.pred-obs)^2)/sum((obs-mean(obs))^2)
sum((mvar.pred.hfs-obs)^2)/sum((obs-mean(obs))^2)
