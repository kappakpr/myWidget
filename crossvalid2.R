#splitting data
# 60% 20% 20% random
# rotation - 5 data point rotation 
# k fold cross validation 80% training 20% testing, usually k=10


#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

# set the margins for plots
par("mar")
par(mar=c(1,1,1,1))


#load the data from CSV file
ccData=read.csv("credit_card_data_header.csv",header=TRUE)

# check the structure & data
str(ccData)
#head(ccData,20)
#verify the result variable
table(ccData$R1)

#all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#create a random index
#ccData_mix = runif(nrow(ccData))
#str(ccData_mix)

#randomized credit card dataset
#ccData2=ccData[order(ccData_mix),]
#str(ccData2)

#changed the result field to a factor
#ccData2$R1=as.factor(ccData2$R1)
#str(ccData2)
#head(ccData2,30)

## --- leave one out one cross-validation --- ##

#ccrows = nrow(ccData2); ccrows
#ccsample = sample(1:ccrows, size = round(ccrows/3), replace = FALSE);ccsample
#cctrain = ccData2[-ccsample,];cctrain
#Test data seleted by including the 2/3rd sample
#cctest = ccData2[ccsample,]  ;cctest

#plotting a couple of variables vs results
#library(ggplot2)          
#ggplot(ccData2, aes(ccData2$A2, ccData2$11, color = ccData2$R)) + geom_point(aes(color = ccData2$R)) + scale_x_continuous("A2", breaks = waiver())+ scale_y_continuous("A15", breaks = waiver())+ theme_bw()+ stat_ellipse() + labs(title="Credit Card Data")

#create a model with training data
#load kknn library
library(kknn)

#scaled = true, distance 1
model_kknn1 = train.kknn(R1 ~ ., ccData, na.action=na.omit, kmax=110, distance=1, 
	kernel=c("triangular", "rectangular", "epanechnikov", "optimal","biweight","triweight","cos","inv","gaussian")
	,scale=TRUE);summary(model_kknn1)

#plot the model
plot(model_kknn1);title("kknn cross validation (distance=1,scaled=true)")

#classification table
pred_1<-predict(model_kknn1, ccData) #; pred_1
pred_table_1<-table(round(pred_1),ccData$R1);pred_table_1
pred_accuracy_1<-sum(diag(pred_table_1)/sum(pred_table_1));pred_accuracy_1
accuracy1<-sum(round(pred_1) == ccData[,11])/nrow(ccData); print (accuracy1)

#scaled = false, distance=1
model_kknn2 = train.kknn(R1 ~ ., ccData, na.action=na.omit, kmax=110, distance=1, 
	kernel=c("triangular", "rectangular", "epanechnikov", "optimal","biweight","triweight","cos","inv","gaussian")
	,scale=FALSE);summary(model_kknn2)

plot(model_kknn2);title("kknn cross validation (distance=1,scaled=false)")

#classification table
pred_2<-predict(model_kknn2, ccData[,1:10])#; pred_2
pred_table_2<-table(round(pred_2),ccData$R1);pred_table_2
pred_accuracy_2<-sum(diag(pred_table_2)/sum(pred_table_2));pred_accuracy_2
accuracy2<-sum(round(pred_2) == ccData[,11])/nrow(ccData); print (accuracy2)

#scaled = true, distance=2
model_kknn3 = train.kknn(R1 ~ ., ccData, na.action=na.omit, kmax=110, distance=2, 
	kernel=c("triangular", "rectangular", "epanechnikov", "optimal","biweight","triweight","cos","inv","gaussian")
	,scale=TRUE);summary(model_kknn3)

plot(model_kknn3);title("kknn cross validation (distance=2,scaled=true)")

#classification table
pred_3<-predict(model_kknn3, ccData[,1:10])#; pred_3
pred_table_3<-table(round(pred_3),ccData$R1);pred_table_3
pred_accuracy_3<-sum(diag(pred_table_3)/sum(pred_table_3));pred_accuracy_3
accuracy3<-sum(round(pred_3) == ccData[,11])/nrow(ccData); print (accuracy3)

#scaled = false, distance = 2
model_kknn4 = train.kknn(R1 ~ ., ccData, na.action=na.omit, kmax=110, distance=2, 
	kernel=c("triangular", "rectangular", "epanechnikov", "optimal","biweight","triweight","cos","inv","gaussian")
	, scale=FALSE);summary(model_kknn4)

plot(model_kknn4);title("kknn cross validation (distance=2, scaled=false)")

#classification table
pred_4<-predict(model_kknn4, ccData[,1:10]) #; pred_4
pred_table_4<-table(round(pred_4),ccData$R1) ;pred_table_4
pred_accuracy_4<-sum(diag(pred_table_4)/sum(pred_table_4));pred_accuracy_4
accuracy4<-sum(round(pred_4) == ccData[,11])/nrow(ccData); print (accuracy4)



##---------- splitting data -----------------##
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
