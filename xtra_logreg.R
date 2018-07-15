rm(list=ls())
set.seed(3)
par(mfrow=c(1,1))
setwd("C:\\RProgs")

xtramdata = read.csv("affair.csv",header=T)

#scatterplot
pairs(log(nbaffairs+1)~ age+ym+religious+education+occupation+rate,data=xtramdata,main="Simple Scatterplot Matrix")
pairs(log(nbaffairs+1)~ sex+child,data=xtramdata,main="Simple Scatterplot sex, child Matrix")
boxplot(log(nbaffairs+1)~sex,ylab = "sex",data = xtramdata)
boxplot(log(nbaffairs+1)~child,ylab = "child",data = xtramdata)

#correlation
xtramdata1=xtramdata[, c(1,3,4,6,7,8,9)]
cor(xtramdata1)

# xtramdata2=xtramdata
# xtramdata2$sex[xtramdata2$sex=="male"]=1
# xtramdata2$sex[xtramdata2$sex=="female"]=0
# xtramdata2$child[xtramdata2$child=="yes"]=1
# xtramdata2$child[xtramdata2$child=="no"]=0
# xtramdata2
# summary(xtramdata2)
# transform(xtramdata2,sex=as.numeric(sex),child=as.numeric(child))
# cor(xtramdata2)

#poisson model 
attach(xtramdata)
m1.pos = glm(nbaffairs ~ ., family="poisson", data=xtramdata)
summary(m1.pos)

# confidence 
confint(m1.pos,level=0.99)

#residual deviance test
with(m1.pos, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE))) 
#check
with(m1.pos, cbind(res.deviance = deviance, df = df.residual, p = 1- pchisq(deviance, df.residual))) 


## Residual Analysis
res.xtra = resid(m1.pos,type="deviance")
par(mfrow=c(2,2))
plot(xtramdata$religious,res.xtra,ylab="Std residuals",xlab="Religious")
abline(0,0,col="blue",lwd=2)
#boxplot(res~religious,ylab = "Std residuals",data = xtramdata)
boxplot(res.xtra~sex,ylab = "Std residuals",data = xtramdata)
qqnorm(res.xtra, ylab="Std residuals")
qqline(res.xtra,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")


# predict new data
pred_data <- data.frame(nbaffairs=0,sex = "male",	age	= 40, ym	=7, child	= "yes" ,religious	= 3, education	= 16, occupation = 6,	rate = 3)
#pred_data1 <- data.frame(nbaffairs=0, sex = "male",	age	= 40, ym	=7, child	= "yes" ,religious	= 3, education	= 16, occupation = 6,	rate = 3)
pred_data$pred = predict(m1.pos,pred_data)
pred_data$pred

prs <- predict(m1.pos, newdata = pred_data, type = "link", se.fit=TRUE)
pred_data$lo <- prs[[1]] - 2.58 * prs[[2]]
pred_data$up <- prs[[1]] + 2.58 * prs[[2]]
pred_data

#predict
#predict(m1.pos,pred_data,interval="predict")
#predict(m1.pos,pred_data,interval="confidence")



ginv <- m1.pos$family$linkinv  ## inverse link function
prs <- predict(m1.pos, newdata = pred_data, type = "link", se.fit=TRUE)
pred_data$pred <- ginv(prs[[1]])
pred_data$lo <- ginv(prs[[1]] - 2.58 * prs[[2]])
pred_data$up <- ginv(prs[[1]] + 2.58 * prs[[2]])

#new data
#new_dat <- rbind(xtramdata,pred_data1)
#m2.pos = glm(nbaffairs ~ ., family="poisson", data=new_dat)
#summary(m2.pos)
