library(kknn)

ccData=read.csv("credit_card_data_header.csv",header=TRUE)

str(ccData)
table(ccData$R1)
head(ccData)
# all Rs on top are 1s

#Rs random number generators for mixing up data
set.seed(9)

ccData_mix = runif(nrow(ccData))

str(ccData_mix)

ccData2=ccData[order(ccData_mix),]

str(ccData2)

ccData2$R1=as.factor(ccData2$R1)

head(ccData2)

rowindices = sample(1:nrow(ccData2), round(.8*nrow(ccData2)), replace=FALSE)

rowindices

##ccData2 = ccData[,-11]

train = as.data.frame(ccData2[rowindices,]);head(train)

str(train)

test = as.data.frame(ccData2[-rowindices,]); head(test)

class(train)

model2 = kknn(R1 ~ ., train , test , na.action=na.omit, k=11, distance=1, kernel='triangular',scale=TRUE)

summary(model2)


iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,kernel = "triangular")


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
