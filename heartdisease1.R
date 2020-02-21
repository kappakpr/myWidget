par(mfrow=c(1,1))
setwd("C:\\RProgs")
hdis = read.csv("fram.csv", head = TRUE, sep = ",")
hdis$SEX = as.factor(hdis$SEX)
hdis$CURSMOKE = as.factor(hdis$CURSMOKE)
summary(hdis)
#We write the variables of interest into two vectors:

pairs(~SEX+AGE+CURSMOKE+BMI,data=hdis,main="Simple Scatterplot Matrix")

hdis.lm = lm(SYSBP~.,data=hdis)
summary(hdis.lm)

confint(hdis.lm,level=0.9)

#modelh$coefficients
#plot(model)
#betas = coef(modelh)
confint(modelh,level=0.95)

hdis30 = hdis[hdis$BMI >= 30,]
summary(hdis30)
nrow(hdis30) 
pairs(~SEX+AGE+CURSMOKE+BMI,data=hdis30,main="30 Simple Scatterplot Matrix")
hdis30.lm = lm(SYSBP~.,data=hdis30)
summary(hdis30.lm)




## Residual Analysis
par(mfrow=c(2,2))
plot(hdis$SYSBP,resid(hdis.lm), main="Predictors vs Residuals")
abline(0,0)
plot(fitted(hdis.lm),resid(hdis.lm),main="Fitted vs Residuals",
     xlab="Fitted Values")
abline(0,0)
qqnorm(resid(hdis.lm),main="QQ-Plot of Residuals")
qqline(resid(hdis.lm))
hist(resid(hdis.lm),main="Histogram of Residuals")


## Residual Analysis 30
par(mfrow=c(2,2))
plot(hdis30$SYSBP,resid(hdis30.lm), main="Predictors vs Residuals")
abline(0,0)
plot(fitted(hdis30.lm),resid(hdis30.lm),main="Fitted vs Residuals",
     xlab="Fitted Values")
abline(0,0)
qqnorm(resid(hdis30.lm),main="QQ-Plot of Residuals")
qqline(resid(hdis30.lm))
hist(resid(hdis30.lm),main="Histogram of Residuals")

#Log model
modelhl = lm(log(rate)~log(signs))
summary(modelhl)
## Residual Analysis
par(mfrow=c(2,2))
plot(log(rate),resid(modelhl), main="Predictors vs Residuals (Log)")
abline(0,0)
plot(fitted(modelhl),resid(modelhl),main="Fitted vs Residuals Log",
     xlab="Fitted Values")
abline(0,0)
qqnorm(resid(modelhl),main="QQ-Plot of Residuals Log")
qqline(resid(modelhl))
hist(resid(modelh),main="Histogram of Residuals Log")

## Prediction Confidence Interval for signs = 2 
signs = 2
predict(modelh,signs)
predict(modelh,signs,interval='prediction',level=.95)

#95%
tvalue=4.600
dof=37
st1=1-pt(tvalue,dof)

## Prediction Confidence Interval for log
signs = log(2)
exp(predict(modelhl,signs))
exp(predict(modelhl,signs,interval='prediction',level=.95))
