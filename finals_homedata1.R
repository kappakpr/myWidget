rm(list=ls())
setwd("C:\\RProgs")

data = read.csv("sales_data_test.csv",header=TRUE)
head(data,10)
summary(data)
str(data)
data = na.omit(data)
attach(data)

data$price_per_foot=data$bid_price/data$home_size

log_model1 = glm(accept_bid ~ . , family = "binomial", data=data)
summary(log_model1)
#457.35

confint(log_model1,level=0.97)

log_model2 = glm(accept_bid ~ . , family = "binomial", data=data)
summary(log_model2)
#454.86


new_data = data.frame(bid_price=630, home_size=2380, type="modern", labor_time=32)
new_data$price_per_foot=new_data$bid_price/new_data$home_size
new_data
predict(log_model2,new_data,type="response",se.fit=TRUE)


new_data1 = data.frame(bid_price=630, home_size=2382, type="modern", labor_time=32)
new_data1$price_per_foot=new_data1$bid_price/new_data1$home_size
new_data1
predict(log_model2,new_data1,type="response",se.fit=TRUE)

new_data2 = data.frame(bid_price=630, home_size=2380, type="modern", labor_time=34)
new_data2$price_per_foot=new_data2$bid_price/new_data2$home_size
new_data2
predict(log_model2,new_data2,type="response",se.fit=TRUE)


deviance(log_model2)
1-pchisq(deviance(log_model2),438.86)

confint(log_model2,level=0.95)

#gstat=log_model2$null.deviance - deviance(log_model2)
#cbind(gstat,1-pchisq(gstat,length(coef(log_model2))-1))
#round(coefficients(summary(log_model2))[,4],4)

set.seed(0)
X = model.matrix(log_model2)
y = data[,1]
X = scale(X, center = TRUE, scale = TRUE)
X[,1] = 1
library(glmnet)
cv = cv.glmnet(X, y,alpha=1,nfolds=10)
cv$lambda.min


lassomdl = glmnet(X, y, family = "gaussian", alpha = 1, lambda = cv$lambda.min)
nnzero(lassomdl$beta)
summary(lassomdl)
coef(lassomdl,s=lassomdl$lambda.min)
