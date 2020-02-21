rm(list=ls())
setwd("C:\\RProgs")

#simulate normal and exponential WN
w1=rnorm(1000,0,1)
w2=rexp(1000,1)

w1
w2
#rescale to mean 0 and std dev 1
w1=(w1-mean(w1))/sqrt(var(w1))
w2=(w2-mean(w2))/sqrt(var(w2))
w1
w2

#plot ts and their acf's
w1=ts(w1,start=1,deltat = 1)
w2=ts(w2,start = 1,deltat = 1)
par(mfrow=c(2,2))
ts.plot(w1,main="Normal")
ts.plot(w2,main="Exponential")
acf(w1,main="Normal ACF")
acf(w2,main="Eponential ACF")


## moving average
rm(list=ls())
setwd("C:\\RProgs")

#simulate white noise
w1=rnorm(502)
w2=rexp(502)-1
#set coefficients
a=c(1,-.5,.2) ## Xt=Zt - 0.5Zt-1 + 0.2Zt-2
a1=c(1,0.5,.2) ## Xt=Zt + 0.5 Zt-1 + 0.2 Zt-2

# above command generates an MA process

ma2.11 = filter(w1,filter = a,side = 1)
ma2.11 = ma2.11[3:502]
ma2.12 = filter(w1,filter = a1,side = 1)
ma2.12 = ma2.12[3:502]
ma2.21 = filter(w2,filter = a,side = 1)
ma2.21 = ma2.21[3:502]
ma2.22 = filter(w2,filter = a1,side = 1)
ma2.22 = ma2.22[3:502]


par(mfrow=c(2,2))
ts.plot(w1,main="Normal")
ts.plot(w2,main="Exponential")
acf(w1,main="Normal ACF")
acf(w2,main="Eponential ACF")

acf(ma2.11,main="Normal -0.5")
acf(ma2.12,main="Normal 0.5")
acf(ma2.21,main="Exponential -0.5")
acf(ma2.22,main="Exponential 0.5")


## moving average non stationary process
rm(list=ls())
setwd("C:\\RProgs")

#white noise simulation
w1=rnorm(502)
#set coeffiecients
a4=c(1,-0.2,0.8,1.2)   ## Xt = Zt - 0.2 Zt-1 + 0.8 Zt-2 + 1.2 Zt-3 with Zt = WN(0,1) * 2t + 0.5
#simulate MA(3) with non stationary noise
ma2.4=filter(w1*(2*(1:502)+0.5),filter=a4,side=1) # multiplying and add operation makes it not stationary
ma2.4=ma2.4[4:502]


par(mfrow=c(2,2))
ts.plot(w1,main="normal moving average WN plot")
ts.plot(ma2.4,main="normal moving average Non Stationary WN plot")
acf(w1,main="normal moving average WN plot ACF")
acf(ma2.4,main="normal moving average NS WN plot ACF")


## AR process
rm(list=ls())
setwd("C:\\RProgs")

## nostationary AR(2)
w2=rnorm(1500)
a2=c(0.8,0.2) ## Xt=0.8Xt-1+0.2Xt-2+Zt
ar2=filter(w2,filter=a2,method='recursive')
ar2=ar2[1251:1500]

## stationary AR(1)
a1=0.5   ## Xt=0.5Xt-1+Zt
ar1=filter(w2,filter=a1,method='recursive')
ar1=ar1[1251:1500]

par(mfrow=c(2,2))
ts.plot(ar1,main="NS AR(1)")
ts.plot(ar2,main="Stationary AR(1)")
acf(ar1,main="")
acf(ar2,main="")

## AR(1) causal process
rm(list=ls())
setwd("C:\\RProgs")

w2=rnorm(1500)

#causal AR(1) processes (|phi| = 0.5 < 1)
a1=0.1
ar1=filter(w2,filter=a1,method='recursive')
ar1.1=ar1[1251:1500]

a1=-0.1
ar1=filter(w2,filter=a1,method='recursive')
ar1.2=ar1[1251:1500]

# causal AR(1) processes reaching non causality / stationary (|phi|=0.9)
a1=0.9
ar1=filter(w2,filter=a1,method='recursive')
ar1.3=ar1[1251:1500]

a1=-0.9
ar1=filter(w2,filter=a1,method='recursive')
ar1.4=ar1[1251:1500]

par(mfrow=c(2,2))
ts.plot(ar1.1,main="phi=0.1")
ts.plot(ar1.2,main="phi=-0.1")
ts.plot(ar1.3,main="phi=0.9")
ts.plot(ar1.4,main="phi=-0.9")

acf(ar1,main="")
acf(ar2,main="")

## AR(2) causal process
rm(list=ls())
setwd("C:\\RProgs")
w2=rnorm(10)
b=c(1.2,-0.5)
ar2=filter(w2,filter=b,method='recursive')
plot(ar2)

## AR(2) causal process
rm(list=ls())
setwd("C:\\RProgs")
w2=rnorm(1500)
b=c(1.2,-0.5)
ar2=filter(w2,filter=b,method='recursive')
ar2=ar2[1001:1500]
data2=data.frame(cbind(x1=ar2[1:498],x2=ar2[2:499],y=ar2[3:500]))
model2=lm(y~x1+x2,data=data2)
summary(model2)
plot(ts(ar2))
pacf(ar2)
pacf(model2$residuals)
plot(ts(model2$residuals))
acf(model2$residuals)


## AR(2) causal process
#rm(list=ls())
#setwd("C:\\RProgs")
covf=acf(ar2,type='covariance',plot=FALSE)
covf
covf$acf
Gamma1 = covf$acf[2:4,,1]
Gamma1
Gammamatrix = matrix(0,3,3)
Gammamatrix
for(i in 1:3){
  if(i>1){
    Gammamatrix[i,] = c(covf$acf[i:2,,1],covf$acf[1:(3-i+1),,1])
  }
  else {
    Gammamatrix[i,] = covf$acf[1:(3-i+1),,1]
  }
}
#estimate phi
phi.estim = solve(Gammamatrix,Gamma1)
phi.estim
