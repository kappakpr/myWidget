#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#load the data from CSV file
ccData=read.csv("credit_card_data_header.csv",header=TRUE)

# check the structure & data
str(ccData)
head(ccData,20)
#verify the result variable
table(ccData$R1)

#all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#create a random index
ccData_mix = runif(nrow(ccData))
str(ccData_mix)

#randomized dataset
ccData2=ccData[order(ccData_mix),]
str(ccData2)

#changed the result field to a factor
ccData2$R1=as.factor(ccData2$R1)
str(ccData2)
head(ccData2,30)

#pick indices to split training and validation data
rowindices = sample(1:nrow(ccData2), round(0.33 * nrow(ccData2)), replace=FALSE); rowindices

#training data
train = as.data.frame(ccData2[-rowindices,]);head(train);str(train)

#validation data
test = as.data.frame(ccData2[rowindices,]); head(test); str(test);class(train)

#create a model with training data
library(kknn)
model_tri = train.kknn(R1 ~ ., train , na.action=na.omit, kmax=110, distance=1, kernel='triangular',scale=TRUE);summary(model_tri)
plot(model_tri);title("Triangular Cross validation")

#Testing the model on test data
pred_tri<-predict(model_tri, test)
pred_table_tri<-table(pred_tri,test$R1);pred_table_tri
pred_accuracy_tri<-sum(diag(pred_table_tri)/sum(pred_table_tri));pred_accuracy_tri

model_re = train.kknn(R1 ~ ., train , na.action=na.omit, kmax=110, distance=1, kernel='rectangular',scale=TRUE);summary(model_re);
plot(model_re);title("Rectangular Cross validation")
#Testing the model on test data
pred_re<-predict(model_re, test)
pred_table_re<-table(pred_re,test$R1);pred_table_re
pred_accuracy_re<-sum(diag(pred_table_re)/sum(pred_table_re));pred_accuracy_re
