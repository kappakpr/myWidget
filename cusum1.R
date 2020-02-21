#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#load data
temps1=read.table("temps.txt",header=TRUE)
str(temps1)

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

summary(temps1)
head(temps1)
table(temps1$X1996)

#valcol <- (crime1$Crime + abs(min(crime1$Crime)))/max(crime1$Crime + abs(min(crime1$Crime)))

#plot without scaling
plot(temps1$X1996,ylab="Temperature")

#plot wit scaling
#plot(scale(crime1$Crime))

