rm(list=ls())
setwd("C:\\RProgs")
set.seed(3)

#load libraries

#load data
data=read.table("breast-cancer-wisconsin.data.txt",header=FALSE, sep=",",stringsAsFactors=FALSE)
data
str(data)

summary(data)
max(as.integer(data$V7))
min(as.integer(data$V7))

missing=which(data$V7 == "?",arr.ind = TRUE)
missing

# mean imputation
mean_v7 = mean(as.numeric(data[-missing,"V7"]))
mean_v7

data_mean_imp = data
data_mean_imp[missing,"V7"] <-as.integer(mean_v7)
#data_mean_imp$V7=as.integer(data_mean_imp$V7)
data_mean_imp[missing,]

#----- regression imputation -------#

#data set excluding the index column, and rows with missing values
data_modified <- data[-missing,2:10]
data_modified$V7=as.integer(data_modified$V7)

#base linear regression model
model0 <- lm(V7~.,data=data_modified)
summary(model0)

#pick only the significant variables and build the model, use this for imputation
model1 <- lm(V7~V2+V4+V5+V9,data=data_modified)
summary(model1)

#predicting imputed values
v7_hat <- predict(model0,newdata = data[missing,])
v7_hat1 <- predict(model1,newdata = data[missing,])
as.integer(v7_hat1)

data_modified_imp = data
data_modified_imp[missing,"V7"] <-as.integer(v7_hat1)
data_modified_imp$V7=as.integer(data_modified_imp$V7)
data_modified_imp[missing,]

#regression with pertubration using rnorm
v7_hat_pert1 <- rnorm(nrow(data[missing,]),v7_hat,sd(v7_hat))
v7_hat_pert1
as.integer(v7_hat_pert1)

data_pert_imp = data
str(data_pert_imp)
data_pert_imp[missing,"V7"] <-as.integer(v7_hat_pert1)
data_pert_imp$V7=as.integer(data_pert_imp$V7)
data_pert_imp[missing,]

# #--- ksvm -- #
# 
# #pick indices to split training and validation/testing data
# rowindices = sample(1:nrow(data_mean_imp), round(0.6 * nrow(data_mean_imp)), replace=FALSE); rowindices
# #training data
# train = as.data.frame(data_mean_imp[rowindices,]);head(train);str(train)
# nrow(train)
# #validation & test data
# valid_test = as.data.frame(data_mean_imp[-rowindices,])#; head(test); str(test);class(train)
# nrow(valid_test)
# # split into 1/2 for validation and testing datasets
# rowindices_valid = sample(1:nrow(valid_test), round(0.5 * nrow(valid_test)), replace=FALSE); rowindices_valid
# #validation
# validation = as.data.frame(valid_test[rowindices_valid,])#; head(validation); str(validation);class(validation)
# nrow(validation)
# #test
# test = as.data.frame(valid_test[-rowindices_valid,])#; head(test); str(test);class(test)
# nrow(test)
# #predictors = as.matrix(train[,2:10]);   classes = as.matrix(train[,11])
# predictors = train[,2:10];   
# predictors$V7=as.integer(predictors$V7)
# classes = train[,11]
# predictors=as.matrix(predictors)
# classes=as.matrix(classes)
# str(predictors)
# str(classes)
# #predict_array = array(0,dim =c(15,7))
# # create a ksvm model with C=100 and kernel=vanilladot
# library(kernlab)
# Cval=1
# Model1 = ksvm(predictors ,classes ,type="C-svc",kernel="vanilladot",C=Cval,scaled=TRUE);Model1 
# a = colSums(predictors[Model1@SVindex,1:9] * Model1@coef[[1]]) #; a
# a0 = - Model1@b; a0
# print(a0)
# str(predictors)
# head(predictors)
# #training data check
# cctrainPrediction = predict(Model1 ,train[,2:10]) ;cctrainPrediction 
# trainsumval=sum(cctrainPrediction == train[,11]) / nrow(train)
# print(c(Cval, trainsumval))