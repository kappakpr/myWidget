rm(list=ls())
setwd("C:\\RProgs")

#R Script for Project#
library(TSA)

data=read.csv("MooseDataVAR.csv",header=T)       
names(data)=c("year", "fairbanksPop", "mooseHarvest", "totalMoose", "avgSnow", "wolfPop") 

years=data[,"year"] 
ts_fairbanksPop=ts(data[,"fairbanksPop"],start=1965, freq=1)
ts_mooseHarvest=ts(data[,"mooseHarvest"],start=1965, freq=1)
ts_totalMoose=ts(data[,"totalMoose"],start=1965, freq=1)
ts_avgSnow=ts(data[,"avgSnow"],start=1965, freq=1)
ts_wolfPop=ts(data[,"wolfPop"],start=1965, freq=1)


##Total Moose Population##
par(mfrow=c(3,2))
plot(diff(ts_fairbanksPop),xlab="Years",ylab="",main="1st Ord Diff: Fairbanks Pop",type="l")
plot(diff(ts_mooseHarvest),xlab="Years",ylab="",main="1st Ord Diff: Moose Harvest",type="l")
plot(diff(ts_totalMoose),xlab="Years",ylab="",main="1st Ord Diff: Moose Pop",type="l")
plot(diff(ts_avgSnow),xlab="Years",ylab="",main="1st Ord Diff: Avg Snow Depth",type="l")
plot(diff(ts_wolfPop),xlab="Years",ylab="",main="1st Ord Diff: Wolf Pop",type="l")

par(mfrow=c(3,2))
plot(diff(ts_fairbanksPop,differences = 2),xlab="Years",ylab="",main="2nd Ord Diff: Fairbanks Pop",type="l")
plot(diff(ts_mooseHarvest,differences = 2),xlab="Years",ylab="",main="2nd Ord Diff: Moose Harvest",type="l")
plot(diff(ts_totalMoose,differences = 2),xlab="Years",ylab="",main="2nd Ord Diff: Moose Pop",type="l")
plot(diff(ts_avgSnow,differences = 2),xlab="Years",ylab="",main="2nd Ord Diff: Avg Snow Depth",type="l")
plot(diff(ts_wolfPop,differences = 2),xlab="Years",ylab="",main="2nd Ord Diff: Wolf Pop",type="l")

##############################################################################################
n = nrow(data)
data.train=data[1:(n-4),]
data.test=data[(n-3):n,]

ts_fairbanksPop=ts(data.train[,"fairbanksPop"],start=1965, freq=1)
ts_mooseHarvest=ts(data.train[,"mooseHarvest"],start=1965, freq=1)
ts_totalMoose=ts(data.train[,"totalMoose"],start=1965, freq=1)
ts_avgSnow=ts(data.train[,"avgSnow"],start=1965, freq=1)
ts_wolfPop=ts(data.train[,"wolfPop"],start=1965, freq=1)

dts_fairbanksPop= diff(ts_fairbanksPop)
dts_mooseHarvest= diff(ts_mooseHarvest)
dts_totalMoose= diff(ts_totalMoose)
dts_avgSnow= diff(ts_avgSnow)
dts_wolfPop= diff(ts_wolfPop)

####################################################################################
library(vars)
###VAR Model##
ddata.train=cbind(dts_fairbanksPop,dts_mooseHarvest,dts_totalMoose, dts_avgSnow,dts_wolfPop)
##Model Selection
VARselect(ddata.train, lag.max = 20)$selection

## Model Fitting: Unrestricted VAR
model.var=VAR(ddata.train, p=4)
summary(model.var)
## Model Fitting: Restricted VAR
model.var.restrict=restrict(model.var)  
summary(model.var.restrict)

## Granger Causality: Wald Test
library(aod)
coef.moosepop = coefficients(model.var)$dts_totalMoose[-(5*4+1),1]
var.model = vcov(model.var)[1:(5*4),1:(5*4)]
## Granger Causality: Fairbanks Population
wald.test(b=coef.moosepop, var.model, Terms=seq(1, 5*4, 5))
## Granger Causality: Moose Harvest
wald.test(b=coef.moosepop, var.model, Terms=seq(2, 5*4, 5))
## Granger Causality: Average Snow Fall
wald.test(b=coef.moosepop, var.model, Terms=seq(4, 5*4, 5))
## Granger Causality: Wolf Population
wald.test(b=coef.moosepop, var.model, Terms=seq(5, 5*4, 5))
## Granger Causality: Wolf Population & Fairbanks Population
wald.test(b=coef.moosepop, var.model, Terms=c(seq(1, 5*4, 5),seq(5, 5*4, 5)))
#######################################################################################
### Reduced VAR Model##
ddata.train=cbind(dts_mooseHarvest,dts_totalMoose, dts_avgSnow)
##Model Selection
VARselect(ddata.train, lag.max = 20)$selection

## Model Fitting: Unrestricted VAR
model.var=VAR(ddata.train, p=5)
summary(model.var)
## Model Fitting: Restricted VAR
model.var.restrict=restrict(model.var)  
summary(model.var.restrict)
## Granger Causality
p=5
nfactor=3
coef.moosepop = coefficients(model.var)$dts_totalMoose[-(nfactor*p+1),1]
var.model = vcov(model.var)[1:(nfactor*p),1:(nfactor*p)]
## Moose Harvest
wald.test(b=coef.moosepop, var.model, Terms=seq(1, nfactor*p, nfactor))
## Snow Fall
wald.test(b=coef.moosepop, var.model, Terms=seq(3, nfactor*p, nfactor))
############################################################################
## Model Forecasting


pred.model=predict(model.var,n.ahead=4)
dmoosepop.fcst = pred.model[[1]]$dts_totalMoose[,1]
final.pred.3 = rep(0,4)
final.pred.3[1] = ts_totalMoose[(n-4)]+dmoosepop.fcst[1]
final.pred.3[2] = final.pred.3[1]+dmoosepop.fcst[2]
final.pred.3[3] = final.pred.3[2]+dmoosepop.fcst[3]
final.pred.3[4] = final.pred.3[3]+dmoosepop.fcst[4]

##############################################################

totalMoose = data[,"totalMoose"]
n = length(totalMoose)
nfit = n-4
train.totalMoose= totalMoose[1:nfit]
ts.train.totalMoose = ts(train.totalMoose,start=1965, freq=1)

## Forecasting Trend+ARMA: 4 years ahead #####
## Predict trend
library(mgcv)
time.pts = c(1:length(totalMoose))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
x = time.pts[1:nfit]
gam.fit.tr = gam(train.totalMoose~s(x))
newdata = data.frame(x=time.pts[(nfit+1):n])
moose.fit.gam = fitted(gam.fit.tr)
moose.fit.gam = ts(moose.fit.gam,start=1965, freq=1)
resid.process = train.totalMoose-moose.fit.gam
resid.process = ts(resid.process,start=1965, freq=1)
gam.pred= predict(gam.fit.tr.4,newdata = newdata,interval=c("prediction"))

## Predict staionary residual process
## Order selection -- AIC 
norder = 6
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
   for(j in 1:norder){
    modij = arima(resid.process,order = c(p[i],0,q[j]), method='ML')
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*nfit/(nfit-p[i]-q[j]-2)
   }  
 }
aicv = as.vector(aic)  
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1 #2
qorder = indexq[indexaic]-1 #3
## Fit ARMA Model
final_model.1 = arima(resid.process,order = c(porder,0,qorder), method='ML')

## Predict ARMA
outpredresid = predict(final_model.1,n.ahead=4)$pred
final.pred.1 = outpredresid+gam.pred

## Forecasting using ARIMA ####
final_model.2 = arima(ts_totalMoose, order = c(porder,1,qorder), method = "ML")
outtotal = arima(ts_train.totalMoose[1:nfit], order = c(porder,1,qorder),method = "ML")
final.pred.2 = as.numeric(predict(outtotal,n.ahead=4)$pred)

######################################################################################333

### Compare Predictions
ts_totalMoose = ts(totalMoose,start=1965, freq=1)
ymin = min(c(ts_totalMoose[(n-20):n],final.pred.1,final.pred.2,final.pred.3))
ymax = max(c(ts_totalMoose[(n-20):n],final.pred.1,final.pred.2,final.pred.3))
plot(years[(n-20):n], ts_totalMoose[(n-20):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Moose Population")
lines(years[(nfit+1):n],final.pred.1,col="red",lwd=2)
lines(years[(nfit+1):n],final.pred.2,col="blue",lwd=2)
lines(years[(nfit+1):n],final.pred.3,col="green",lwd=2)
legend(1987,16000,legend=c("Trend+ARMA","ARIMA","VAR"),col=c("red","blue","green"),lty=1)

