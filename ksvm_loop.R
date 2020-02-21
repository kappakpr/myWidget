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

predictors = as.matrix(ccData[,1:10]);   classes = as.matrix(ccData[,11])
predict_array = array(0,dim =c(10,7))

library(kernlab)

i = 1
for (Cval in c(0.000005,0.0005,0.005,.01,0.2,0.5,5,10,100,150))
{
	ccAppModel1 = ksvm(predictors ,classes ,type="C-svc",kernel="vanilladot",C=Cval,scaled=TRUE);ccAppModel1 
	a = colSums(predictors[ccAppModel1@SVindex,1:10] * ccAppModel1@coef[[1]])
	a0 = - ccAppModel1@b; a0
	ccPrediction = predict(ccAppModel1 ,ccData[,1:10])
	sumval=sum(ccPrediction == ccData[,11]) / nrow(ccData)

	pred<-predict(ccAppModel1,ccData[,-11]); pred
	pred_table<-table(pred,ccData$R1);pred_table
	pred_accuracy<-sum(diag(pred_table)/sum(pred_table)) #;pred_accuracy

	predict_array[i,1] = Cval
	predict_array[i,2] = pred_accuracy
	predict_array[i,3] = sumval
	predict_array[i,4] = a0
	i = i + 1
}

print(predict_array)
