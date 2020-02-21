#Clear environment
rm(list=ls())

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#to export output to a file
library(TeachingDemos)

#set working directory
setwd("C:\\RProgs")

#set plot directory
#jpeg(file = "C:\\RProgs\\mycrimedataplots.jpeg")

#set console width
options(width=500) 

#load data
attemps=read.table("temps.txt",header=TRUE)

#create vector
attempsvec = as.vector(unlist(attemps[2:21]))
attempsvec # has 2460 data points

#create timeseries
#data is for 123 days, every 123 days we get a new year's data
attempsts = ts(attempsvec,start=1996,frequency=123)
attempsts # has 2460 data points

# additive model , will calculate optimum value for alpha, beta, gamma
expmodel = HoltWinters(attempsts)
txtStart("expmodel.txt")
expmodel
expmodel$fitted
expmodel$SSE
txtStop()

plot(expmodel$fitted)

m <- matrix(expmodel$fitted[,4],nrow=123)
txtStart("expmodelmatrix.txt")
m
txtStop()

#multiplicative model
expmodelmul = HoltWinters(attempsts, seasonal="multiplicative")
txtStart("expmodelmul.txt")
expmodelmul
expmodelmul$fitted
expmodelmul$SSE
txtStop()

plot(expmodelmul$fitted)

mmul <- matrix(expmodelmul$fitted[,4],nrow=123)
txtStart("expmodelmulmatrix.txt")
mmul
txtStop()

# model with a low value of alpha 0.07 to reduce the effect of randomness
expmodel07 = HoltWinters(attempsts, alpha = 0.07)
txtStart("expmodel07.txt")
expmodel07
expmodel07$fitted
expmodel07$SSE
txtStop()

plot(expmodel07)
plot(expmodel07$fitted)

m07 <- matrix(expmodel07$fitted[,4],nrow=123)
txtStart("expmodel07matrix.txt")
m07
txtStop()

# model with a low value of alpha 0.07 to reduce the effect of randomness

expmodel001 = HoltWinters(attempsts, alpha = 0.01)
txtStart("expmodel001.txt")
expmodel001
expmodel001$fitted
expmodel001$SSE
txtStop()

plot(expmodel001)
plot(expmodel001$fitted)

# model with beta set to FALSE as there is no trend

expmodelbetaF = HoltWinters(attempsts, alpha = 1, beta=FALSE)
txtStart("expmodelbetaF.txt")
expmodelbetaF
expmodelbetaF$fitted
expmodelbetaF$SSE
txtStop()

plot(expmodelbetaF)
plot(expmodelbetaF$fitted)

# model with gamma set to FALSE to remove seasonality

expmodelgammaF = HoltWinters(attempsts, alpha = 1, gamma=FALSE)
txtStart("expmodelgammaF.txt")
expmodelgammaF
expmodelgammaF$fitted
expmodelgammaF$SSE
txtStop()

plot(expmodelgammaF)
plot(expmodelgammaF$fitted)

#mgammaF <- matrix(expmodelgammaF$fitted[,2],nrow=123)
#txtStart("expmodelgammaFmatrix.txt")
#mgammaF
#txtStop()

# mean temperature of july data for each row
library(dplyr)
baseline <- attemps %>% filter(grepl("Jul",DAY)) %>% select(-DAY) %>% rowMeans()

mu <- mean(baseline)
sd <- sd(baseline)

cumdata1 <- data.frame (index = 1:nrow(attemps),
		day = attemps %>% select (DAY) %>% unlist(),
		avg = attemps %>% select (-DAY) %>% rowMeans(),
		row.names = NULL)

