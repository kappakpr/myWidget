# manual k fold validation with kknn

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
head(ccData,20)
#verify the result variable
table(ccData$R1)

#all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

# Assign data/row numbers to fold groups
kfolds <- 10

cv.sampling <- sample(x=kfolds, size = nrow(ccData), replace = T)

# create an empty vector
predictedvalue <- rep(0, nrow(ccData))

for (currentfold in 1:kfolds) {
  
  prediction <- kknn(R1~., 
                     train = ccData[cv.sampling!=currentfold,],
                     test = ccData[cv.sampling==currentfold,],
                     k=40, kernel = "gaussian")
  
  predictedvalue[cv.sampling==currentfold] <- round(prediction$fitted.values)
}

#classification accuracy
accuracy1<-sum(predictedvalue == ccData[,11])/nrow(ccData); print (accuracy1)

# classification table & compute accuracy from table
pred_table_1<-table(round(predictedvalue),ccData$R1);pred_table_1
pred_accuracy_1<-sum(diag(pred_table_1)/sum(pred_table_1));pred_accuracy_1
