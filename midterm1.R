rm(list=ls())
setwd("C:\\RProgs")
data = read.csv("sales_data_test.csv",header=TRUE,sep=",")
summary(data)

#remove outlier #18
data2 = data
data2<-data2[-18,]
data2
summary(data2)

#scatterplot
pairs(Lumber_sales~.,data=data,main="Simple Scatterplot Matrix")

#model 1
model1 = lm(Lumber_sales~.,data=data)
summary(model1)

summary(model1)$sigma

par(mfrow=c(2,2))
plot(model1)
confint(model1)

#cooks distance
cook = cooks.distance(model1)
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
par(mfrow=c(2,2))
plot(model1,cook.levels = c(4/20,0.5,1))

#model 1
model2 = lm(Lumber_sales~.,data=data2)
summary(model2)

#predict for median_hh_inc = 32000, stock_availability = 0.24, min_wage = 7.25, competitor_sales = 650, and price_elasticity = 1.04.
pred_data <- data.frame(median_hh_inc = 32000, stock_availability = 0.24
                        , min_wage = 7.25, competitor_sales = 650, price_elasticity = 1.04)
predict(model2,pred_data)

s2=summary(model2)$sigma^2
s2
xstar=as.double(pred_data)
xstar
pred_data
data
X=data.matrix(data[,2:6])
X
predvar=s2*(1+xstar%*%solve(t(X)%*%X)%*%xstar)
predvar
predict(model2,pred_data,interval = "prediction")
predict(model2,pred_data,interval = "confidence")
