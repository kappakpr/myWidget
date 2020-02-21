#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#set plot directory
#jpeg(file = "C:\\RProgs\\mycrimedataplots.jpeg")

#load data
uscrime=read.table("uscrime.txt",header=TRUE)
uscrime1=uscrime
#str(uscrime1)

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#summary(uscrime1)
#head(uscrime1)
#table(uscrime1$Crime)
summary(uscrime1$Crime)
sd(uscrime1$Crime)
#install.packages("outliers")

cat ("The minimum value and index of Crime ", min(uscrime1$Crime), which.min(uscrime1$Crime))
cat (" The maximum value and index of Crime  ", max(uscrime1$Crime), which.max(uscrime1$Crime))
print (" ")
  
library(car)
Boxplot(uscrime1$Crime, main = "US Crime data of number of offenses per 100,000 population" 
, ylab = "Number of offences per 100,000 population",col="grey", outcol="red",id.n = Inf)
boxplot.stats(uscrime1$Crime)$out
  
crime=uscrime1$Crime
#plot without scaling
plot(crime ,ylab="Crime data for US"
	,main = "US crime data without scaling",pch=20,cex=1.7
	,col=ifelse(crime==min(crime) | crime==max(crime)
      ,"darkred", "darkgreen"))
  
text(which(crime==min(crime)),min(crime),labels=min(crime),pos=2)
text(which(crime==max(crime)),max(crime),labels=max(crime),pos=2)

qqnorm(crime)
qqnorm(scale(crime))


#plot wit scaling
#plot(scale(uscrime1$Crime),ylab="Crime data for US",main = "US crime data with scaling")
  
library(outliers)
# one side outlier
#largest
g1 = grubbs.test(uscrime1$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
print(g1)
  
  
#smallest
g2=grubbs.test(uscrime1$Crime, type = 10, opposite = TRUE, two.sided = FALSE)
print(g2)
print(g2$p.value)

#loop through grubbs test, removing the highest outlier each time
for (i in 1:10)
{
  
uscrime1 = uscrime1[-which.max(uscrime1$Crime),]
summary(uscrime1$Crime)

g1 = grubbs.test(uscrime1$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
print(g1)
  
  if (g1$p.value > 0.1) # & g2$p.value > 0.1) 
  {
    print ("exit the loop")
    break
  }
    
}

crime=uscrime1$Crime
#plot without scaling
plot(crime ,ylab="Crime data for US"
	,main = "US crime data without scaling",pch=20,cex=1.7
	,col=ifelse(crime==min(crime) | crime==max(crime)
      ,"darkred", "darkgreen"))
  
text(which(crime==min(crime)),min(crime),labels=min(crime),pos=2)
text(which(crime==max(crime)),max(crime),labels=max(crime),pos=2)

qqnorm(crime )
summary(uscrime1$Crime)
sd(uscrime1$Crime)

#if(!is.null(dev.list())) dev.off()
