#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#load data
ccData=read.csv("credit_card_data_header.csv",header=TRUE)
str(ccData)

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#pick indices to split training and validation/testing data
rowindices = sample(1:nrow(ccData), round(0.6 * nrow(ccData)), replace=FALSE); rowindices

#training data
train = as.data.frame(ccData[rowindices,]);head(train);str(train)
nrow(train)

#validation & test data
valid_test = as.data.frame(ccData[-rowindices,])#; head(test); str(test);class(train)
nrow(valid_test)

# split into 1/2 for validation and testing datasets
rowindices_valid = sample(1:nrow(valid_test), round(0.5 * nrow(valid_test)), replace=FALSE); rowindices_valid

#validation
validation = as.data.frame(valid_test[rowindices_valid,])#; head(validation); str(validation);class(validation)
nrow(validation)

#test
test = as.data.frame(valid_test[-rowindices_valid,])#; head(test); str(test);class(test)
nrow(test)

predictors = as.matrix(train[,1:10]);   classes = as.matrix(train[,11])
#predict_array = array(0,dim =c(15,7))

# create a ksvm model with C=100 and kernel=rbfdot
library(kernlab)
Cval=100
ccAppModel1 = ksvm(predictors ,classes ,type="C-svc",kernel="rbfdot",C=Cval,scaled=TRUE);ccAppModel1 
a = colSums(predictors[ccAppModel1@SVindex,1:10] * ccAppModel1@coef[[1]]) #; a
a0 = - ccAppModel1@b; a0
print(a0)

#training data check
cctrainPrediction = predict(ccAppModel1 ,train[,1:10]) ;cctrainPrediction 
trainsumval=sum(cctrainPrediction == train[,11]) / nrow(train)
print(c(Cval, trainsumval))

trainpred<-predict(ccAppModel1,train[,-11]); trainpred
pred_table_train<-table(trainpred,train$R1);
print(pred_table_train)
pred_accuracy_train<-sum(diag(pred_table_train)/sum(pred_table_train)) ;pred_accuracy_train
#print (1-ccAppModel1 )
#str(ccAppModel1)

#validation data check
ccvalidPrediction = predict(ccAppModel1 ,validation[,1:10]) ;ccvalidPrediction 
validsumval=sum(ccvalidPrediction == validation[,11]) / nrow(validation)
print(c(Cval, validsumval))

validpred<-predict(ccAppModel1,validation[,-11]); validpred
pred_table_valid<-table(validpred,validation$R1);
print(pred_table_valid)
pred_accuracy_valid<-sum(diag(pred_table_valid)/sum(pred_table_valid)) ;pred_accuracy_valid


#test data check 
cctestPrediction = predict(ccAppModel1 ,test[,1:10]) ;cctestPrediction 
testsumval=sum(cctestPrediction == test[,11]) / nrow(test)
print(c(Cval, testsumval))

testpred<-predict(ccAppModel1,test[,-11]); testpred
pred_table_test<-table(testpred,test$R1);
print(pred_table_test)
pred_accuracy_test<-sum(diag(pred_table_test)/sum(pred_table_test)) ;pred_accuracy_test