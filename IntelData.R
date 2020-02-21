rm(list=ls())
setwd("C:\\RProgs")


library(data.table) 
#Load data
intel=read.csv("IntelData.csv",header=TRUE)

#Split data into training and testing
intel=intel[,c(2,3,4)]
intel.train=intel[1:(nrow(intel)-4),]
intel.test=intel[(nrow(intel)-3):nrow(intel),]

ts_intel=ts(intel.train,start = 1,frequency = 1)
plot(ts_intel)
acf(ts_intel)