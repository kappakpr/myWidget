library(kernlab)
ccData=read.csv("credit_card_data_header.csv",header=TRUE)
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
