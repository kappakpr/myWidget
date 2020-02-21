#read the dataset
rm(list=ls())
setwd("C:\\RProgs")
set.seed(3)
data=read.csv("concrete.csv",header=T)
library(psycho)
data=standardize(data)

#split train, test
rowindices = sample(1:nrow(data), round(0.8 * nrow(data)), replace=FALSE); rowindices
train=data[rowindices,]
test=data[-rowindices,]
nrow(train)
n=nrow(test);n
attach(train)

p=8 # number of predictors

#setup a matrix with all predictors minus intercept, vector with the result
x <- model.matrix(Y~., train)[,-1]
y <- train$Y

u <- model.matrix(Y~., test)[,-1]
v <- test$Y


library(glmnet)
#--ridge
ridge = cv.glmnet(x,y, family = "gaussian", alpha = 0)
#ridge

yhat_ridge = predict(ridge, u)
mse_ridge = sum((v-yhat_ridge)^2)/n; mse_ridge
library(Metrics)
rmse_ridge=rmse(yhat_ridge, v); rmse_ridge
nrow(yhat_ridge)
coef(ridge,"lambda.min")

#--lasso
lasso = cv.glmnet(x,y, family = "gaussian", alpha = 1)
lasso
coef(lasso,"lambda.min")
coef(lasso)

lambda = lasso$lambda.min
lambda
yhat_lasso = predict(lasso, u, s = lambda)
mse_lasso = sum((v-yhat_lasso)^2)/n; mse_lasso
nrow(yhat_lasso)
rmse(v,yhat_lasso)


#adaptive lasso regression

gamma = 2
b.ols = solve(t(x)%*%x)%*%t(x)%*%y
l.ridge = ridge$lambda.min
b.ridge = matrix(coef(ridge, s = l.ridge))[2:(p+1)]
w1 = 1/abs(b.ols)^gamma
w2 = 1/abs(b.ridge)^gamma
alasso1 = cv.glmnet(x, y, family = "gaussian", alpha = 1, penalty.factor = w1) #cross validation
alasso2 = cv.glmnet(x, y, family = "gaussian", alpha = 1, penalty.factor = w2)
lambda1 = alasso1$lambda.min; lambda1
lambda2 = alasso2$lambda.min; lambda2
coef.alasso1 = matrix(coef(alasso1, s = lambda1))[2:(p+1)]
coef.alasso2 = matrix(coef(alasso2, s = lambda2))[2:(p+1)]
alasso1 = glmnet(x, y, family = "gaussian", alpha = 1, penalty.factor = w1)
alasso2 = glmnet(x, y, family = "gaussian", alpha = 1, penalty.factor = w2)

alasso1$lambda
coef(alasso1)

y_alasso1 = predict(alasso1, u, s = lambda1)
mse_alasso1 = sum((v-y_alasso1)^2)/n; mse_alasso1
rmse(v,y_alasso1)
y_alasso2 = predict(alasso2, u, s = lambda2)
mse_alasso2 = sum((v-y_alasso2)^2)/n; mse_alasso2
rmse(v,y_alasso2)

#elastic net regression

library(caret)
library(tidyverse)
elasticnet <- train(
  Y ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
elasticnet$bestTune


coef(elasticnet$finalModel, elasticnet$bestTune$lambda)


# Make predictions on the test data
#x.test <- model.matrix(medv ~., test.data)[,-1]
predictions <- elasticnet %>% predict(u)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, v),
  Rsquare = R2(predictions, v)
)

mse_elstc = sum((v-predictions)^2)/n; mse_elstc

elasticnet
#which model to select ?
