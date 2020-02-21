rm(list=ls())
set.seed(3)
par(mfrow=c(1,1))
setwd("C:\\RProgs")
#bwt = read.csv("birthwt1.csv", head = TRUE, sep = ",",stringsAsFactors = FALSE)
bwt = read.csv("birthwt1.csv", head = TRUE, sep = ",")

pairs(BirthWt~Age+MotherWt+NumPreLabor+NumPhysicianVt,data=bwt,main="Simple Scatterplot Matrix")
par(mfrow=c(2,2))
boxplot(bwt$BirthWt~as.factor(bwt$Race),xlab="Race", ylab="BirthWt",outcol="red",id.n = Inf)
boxplot(bwt$BirthWt~as.factor(bwt$Smoke), xlab="Smoke", ylab="BirthWt",outcol="red",id.n = Inf)
boxplot(bwt$BirthWt~as.factor(bwt$Hypertension), xlab="Hypertension", ylab="BirthWt",outcol="red",id.n = Inf)
boxplot(bwt$BirthWt~as.factor(bwt$UterineIrr), xlab="Uterine Irritability", ylab="BirthWt",outcol="red",id.n = Inf) 




#model
bwt.lm = lm(BirthWt~.,data=bwt)
summary(bwt.lm)
par(mfrow=c(2,2))
plot(bwt.lm)
#confidence intervals
confint(bwt.lm,level=0.95)

cook = cooks.distance(bwt.lm)
par(mfrow=c(1,1))
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
#outliers
bwt[3,]
bwt[153,]
bwt[189,]

par(mfrow=c(2,2))
plot(bwt.lm,cook.levels = c(4/189,0.5,1))
plot(bwt.lm,cook.levels = c(0.5,1))

## Residual Analysis
par(mfrow=c(2,2))
plot(bwt.lm)

par(mfrow=c(2,2))
plot(bwt$MotherWt,resid(bwt.lm), main="MotherWt vs Residuals")
abline(0,0)
#plot race vs residuals
bwt2=bwt
bwt2$RaceInt[bwt$Race=="White"]<-1
bwt2$RaceInt[bwt$Race=="Black"]<-0
bwt2$RaceInt[bwt$Race=="Other"]<-3
plot(bwt2$RaceInt,resid(bwt.lm), main="Race vs Residuals")
abline(0,0)
bwt2$SmokeInt[bwt$Smoke=="Yes"]<-1
bwt2$SmokeInt[bwt$Smoke=="No"]<-0
plot(bwt2$SmokeInt,resid(bwt.lm), main="Smoke vs Residuals")
abline(0,0)
bwt2$UterineInt[bwt$UterineIrr=="Yes"]<-1
bwt2$UterineInt[bwt$UterineIrr=="No"]<-0
plot(bwt2$UterineInt,resid(bwt.lm), main="UterineIrr vs Residuals")
abline(0,0)


par(mfrow=c(1,1))
plot(bwt.lm, which = 1:4)


bwt1=bwt[-3,]
bwt1.lm = lm(BirthWt~.,data=bwt1)
summary(bwt1.lm)
anova(bwt1.lm, bwt.lm)

library(outliers)
# one side outlier
#largest
g1 = grubbs.test(bwt$BirthWt, type = 10, opposite = FALSE, two.sided = FALSE);
print(g1)





bwt.lm$coefficients
#library(car)
#BoxPlot(bwt$BirthWt~bwt$Age,xlab="Age", ylab="BirthWt",outcol="red",id.n = Inf)
#outlier_values <- boxplot.stats(bwt$BirthWt)$out

#hist(bwt$Race,main="Histogram of Race")
#bwt$Race[bwt$Race=="Other"] <- 0
#bwt$Race[bwt$Race == "White"] <- 1
#bwt$Race[bwt$Race == "Black"] <- 2
#bwt$Race = as.factor(bwt$Race)
