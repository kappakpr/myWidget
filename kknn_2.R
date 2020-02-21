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

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

# create a random index
ccData_mix = runif(nrow(ccData))
str(ccData_mix)

# randomized dataset
ccData2=ccData[order(ccData_mix),]
str(ccData2)

# changed the result field to a factor
ccData2$R1=as.factor(ccData2$R1)
str(ccData2)
head(ccData2,30)


#pick indices to split training and validation data
rowindices = sample(1:nrow(ccData2), round(0.33 * nrow(ccData2)), replace=FALSE); rowindices

##ccData2 = ccData[,-11]

#training data
train = as.data.frame(ccData2[-rowindices,]);head(train);str(train)

#validation data
test = as.data.frame(ccData2[rowindices,]); head(test); str(test);class(train)

#create a model with training data
library(kknn)
model2 = train.kknn(R1 ~ ., train , na.action=na.omit, kmax=110, distance=1, kernel='triangular',scale=TRUE);summary(model2)
plot(model2);title("Cross validation")

model2accuracy = sum(model2$fitted.values ==  test[,11]) / length(test[,11])


model_re = train.kknn(R1 ~ ., train , na.action=na.omit, kmax=110, distance=1, kernel='rectangular',scale=TRUE);summary(model_re);plot(model_re);title("Cross validation")

#Testing the model on test data
pred<-predict(model2, test)
#pred_bin<-round(pred)


pred_accuracy<-table(pred,test$R1)
pred_accuracy


model3 = kknn(R1 ~ ., train, test, na.action=na.omit, k=11, distance=2, kernel='inv',scale=TRUE)

summary(model2)

model2accuracy = sum(model2$fitted.values ==  test[,11]) / length(test[,11])

model3accuracy = sum(model3$fitted.values ==  test[,11]) / length(test[,11])

model2accuracy

model3accuracy 


kknn(formula = formula(train), train, test, na.action = na.omit(), 
	k = 7, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
	contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))
kknn.dist(learn, valid, k = 10, distance = 2)

predictors = as.matrix(ccData[,1:10]);   classes = as.matrix(ccData[,11])

predict_array = numeric(length(10))
i = 0
Cval = 5
for (Cval in c(0.0002,0.001,0.01,0.2,0.5,5))
{
	ccAppModel1 = ksvm(predictors ,classes ,type="C-svc",kernel="vanilladot",C=Cval,scaled=TRUE)
	a = colSums(predictors[ccAppModel1@SVindex,1:10] * ccAppModel1@coef[[1]]) ; a
	a0 = - ccAppModel1@b; a0
	print(a0)
	ccPrediction = predict(ccAppModel1 ,ccData[,1:10]) #;ccPrediction 
	sumval=sum(ccPrediction == ccData[,11]) / nrow(ccData)
	print(c(Cval, sumval))
	predict_array[i] = sumval
	i = i + 1
}
#print(predict_array[i])
