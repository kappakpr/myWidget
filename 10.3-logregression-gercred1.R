#Clear environment
rm(list=ls())

library(pROC)

#set working directory
setwd("C:\\RProgs")

set.seed(3)

#par(mar=c(1,1,1,1))

#set plot directory
#jpeg(file = "C:\\RProgs\\mycrimedataplots.jpeg")

#load data
gercredit=read.table("germancredit.txt", stringsAsFactors = FALSE)
gercredit1=gercredit
summary(gercredit1)
head(gercredit1)
gercredit1$V21[gercredit1$V21 == 1] = 0
gercredit1$V21[gercredit1$V21 == 2] = 1

#split training / validation
numrow=nrow(gercredit1)
train_sample=sample(1:numrow, size = round(0.7 * numrow), replace = FALSE)
gercredit1_train=gercredit1[train_sample,]
gercredit1_valid=gercredit1[-train_sample,]

mod1 <- glm(V21~.,data=gercredit1_train,family=binomial(link="logit"))
summary(mod1)

mod2 <- glm(gercredit1_train$V21~V1+V2+V3+V4+V6+V8+V9+V10+V13+V14+V20,data=gercredit1_train,family=binomial(link="logit"))
summary(mod2)

#translate the categorical variables
gercredit1_train$V1A13[gercredit1_train$V1 == "A13"] = 1
gercredit1_train$V1A13[gercredit1_train$V1 != "A13"] = 0
gercredit1_train$V1A14[gercredit1_train$V1 == "A14"] = 1
gercredit1_train$V1A14[gercredit1_train$V1 != "A14"] = 0
gercredit1_train$V3A32[gercredit1_train$V3 == "A32"] = 1
gercredit1_train$V3A32[gercredit1_train$V3 != "A32"] = 0
gercredit1_train$V3A33[gercredit1_train$V3 == "A33"] = 1
gercredit1_train$V3A33[gercredit1_train$V3 != "A33"] = 0
gercredit1_train$V3A34[gercredit1_train$V3 == "A34"] = 1
gercredit1_train$V3A34[gercredit1_train$V3 != "A34"] = 0
gercredit1_train$V4A41[gercredit1_train$V4 == "A41"] = 1
gercredit1_train$V4A41[gercredit1_train$V4 != "A41"] = 0
gercredit1_train$V4A410[gercredit1_train$V4 == "A410"] = 1
gercredit1_train$V4A410[gercredit1_train$V4 != "A410"] = 0
gercredit1_train$V4A42[gercredit1_train$V4 == "A42"] = 1
gercredit1_train$V4A42[gercredit1_train$V4 != "A42"] = 0
gercredit1_train$V4A43[gercredit1_train$V4 == "A43"] = 1
gercredit1_train$V4A43[gercredit1_train$V4 != "A43"] = 0
gercredit1_train$V6A62[gercredit1_train$V6 == "A62"] = 1
gercredit1_train$V6A62[gercredit1_train$V6 != "A62"] = 0
gercredit1_train$V6A64[gercredit1_train$V6 == "A64"] = 1
gercredit1_train$V6A64[gercredit1_train$V6 != "A64"] = 0
gercredit1_train$V6A65[gercredit1_train$V6 == "A65"] = 1
gercredit1_train$V6A65[gercredit1_train$V6 != "A65"] = 0
gercredit1_train$V7A73[gercredit1_train$V7 == "A73"] = 1
gercredit1_train$V7A73[gercredit1_train$V7 != "A73"] = 0
gercredit1_train$V7A74[gercredit1_train$V7 == "A74"] = 1
gercredit1_train$V7A74[gercredit1_train$V7 != "A74"] = 0
gercredit1_train$V7A75[gercredit1_train$V7 == "A75"] = 1
gercredit1_train$V7A75[gercredit1_train$V7 != "A75"] = 0
gercredit1_train$V9A92[gercredit1_train$V9 == "A92"] = 1
gercredit1_train$V9A92[gercredit1_train$V9 != "A92"] = 0
gercredit1_train$V9A93[gercredit1_train$V9 == "A93"] = 1
gercredit1_train$V9A93[gercredit1_train$V9 != "A93"] = 0
gercredit1_train$V9A94[gercredit1_train$V9 == "A94"] = 1
gercredit1_train$V9A94[gercredit1_train$V9 != "A94"] = 0
gercredit1_train$V10A103[gercredit1_train$V10 == "A103"] = 1
gercredit1_train$V10A103[gercredit1_train$V10 != "A103"] = 0
gercredit1_train$V14A143[gercredit1_train$V14 == "A143"] = 1
gercredit1_train$V14A143[gercredit1_train$V14 != "A143"] = 0

#model with new categoric variables
mod3 <- glm(gercredit1_train$V21~V1A13+V1A14+V2+V3A32+V3A33+V3A34+V4A41+V4A410+V4A42+V4A43+V5+V6A62+V6A64+V6A65+V7A73+V7A74+V7A75+V8+V9A92+V9A93+V9A94+V10A103+V14A143+V16,data=gercredit1_train,family=binomial(link="logit"))
summary(mod3)

mod4 <- glm(gercredit1_train$V21~V1A13+V1A14+V2+V3A32+V3A33+V3A34+V4A41+V4A410+V4A43+V5+V6A62+V6A64+V6A65+V7A74+V7A75+V8+V9A92+V9A93+V10A103+V14A143,data=gercredit1_train,family=binomial(link="logit"))
summary(mod4)

mod5 <- glm(gercredit1_train$V21~V1A13+V1A14+V2+V3A32+V3A33+V3A34+V4A41+V4A410+V4A43+V5+V6A62+V6A64+V6A65+V8+V9A93+V10A103,data=gercredit1_train,family=binomial(link="logit"))
summary(mod5)

#translate the categorical variables
gercredit1_valid$V1A13[gercredit1_valid$V1 == "A13"] = 1
gercredit1_valid$V1A13[gercredit1_valid$V1 != "A13"] = 0
gercredit1_valid$V1A14[gercredit1_valid$V1 == "A14"] = 1
gercredit1_valid$V1A14[gercredit1_valid$V1 != "A14"] = 0
gercredit1_valid$V3A32[gercredit1_valid$V3 == "A32"] = 1
gercredit1_valid$V3A32[gercredit1_valid$V3 != "A32"] = 0
gercredit1_valid$V3A33[gercredit1_valid$V3 == "A33"] = 1
gercredit1_valid$V3A33[gercredit1_valid$V3 != "A33"] = 0
gercredit1_valid$V3A34[gercredit1_valid$V3 == "A34"] = 1
gercredit1_valid$V3A34[gercredit1_valid$V3 != "A34"] = 0
gercredit1_valid$V4A41[gercredit1_valid$V4 == "A41"] = 1
gercredit1_valid$V4A41[gercredit1_valid$V4 != "A41"] = 0
gercredit1_valid$V4A410[gercredit1_valid$V4 == "A410"] = 1
gercredit1_valid$V4A410[gercredit1_valid$V4 != "A410"] = 0
gercredit1_valid$V4A42[gercredit1_valid$V4 == "A42"] = 1
gercredit1_valid$V4A42[gercredit1_valid$V4 != "A42"] = 0
gercredit1_valid$V4A43[gercredit1_valid$V4 == "A43"] = 1
gercredit1_valid$V4A43[gercredit1_valid$V4 != "A43"] = 0
gercredit1_valid$V6A62[gercredit1_valid$V6 == "A62"] = 1
gercredit1_valid$V6A62[gercredit1_valid$V6 != "A62"] = 0
gercredit1_valid$V6A64[gercredit1_valid$V6 == "A64"] = 1
gercredit1_valid$V6A64[gercredit1_valid$V6 != "A64"] = 0
gercredit1_valid$V6A65[gercredit1_valid$V6 == "A65"] = 1
gercredit1_valid$V6A65[gercredit1_valid$V6 != "A65"] = 0
gercredit1_valid$V7A73[gercredit1_valid$V7 == "A73"] = 1
gercredit1_valid$V7A73[gercredit1_valid$V7 != "A73"] = 0
gercredit1_valid$V7A74[gercredit1_valid$V7 == "A74"] = 1
gercredit1_valid$V7A74[gercredit1_valid$V7 != "A74"] = 0
gercredit1_valid$V7A75[gercredit1_valid$V7 == "A75"] = 1
gercredit1_valid$V7A75[gercredit1_valid$V7 != "A75"] = 0
gercredit1_valid$V9A92[gercredit1_valid$V9 == "A92"] = 1
gercredit1_valid$V9A92[gercredit1_valid$V9 != "A92"] = 0
gercredit1_valid$V9A93[gercredit1_valid$V9 == "A93"] = 1
gercredit1_valid$V9A93[gercredit1_valid$V9 != "A93"] = 0
gercredit1_valid$V9A94[gercredit1_valid$V9 == "A94"] = 1
gercredit1_valid$V9A94[gercredit1_valid$V9 != "A94"] = 0
gercredit1_valid$V10A103[gercredit1_valid$V10 == "A103"] = 1
gercredit1_valid$V10A103[gercredit1_valid$V10 != "A103"] = 0
gercredit1_valid$V14A143[gercredit1_valid$V14 == "A143"] = 1
gercredit1_valid$V14A143[gercredit1_valid$V14 != "A143"] = 0

pred_21<-predict(mod5,gercredit1_valid,type="response")
bin_pred_21 = as.integer(pred_21 > 0.5)
tab=table(bin_pred_21,gercredit1_valid$V21)
acc1=(tab[1,1] + tab[2,2]) / sum(tab)

roc4 = roc(gercredit1_valid$V21,pred_21)
plot(roc4)

bin_pred_21 = as.integer(pred_21 > 0.2)
roc4 = roc(gercredit1_valid$V21,bin_pred_21)
plot(roc4)

bin_pred_21 = as.integer(pred_21 > 0.05)
tab=table(bin_pred_21,gercredit1_valid$V21)
acc1=(tab[1,1] + tab[2,2]) / sum(tab)
err = 5 * tab[1,2] + tab[2,1];print (err)

thres <- seq(0.15,0.95,by=0.05)
acc <- rep(0,length(thres))
A.U.C <- rep(0,length(thres))


for (i in 1:length(thres)) {
  t <- thres[i]
  bin_pred <- as.integer(pred_21 > t)
  conf <- table(bin_pred, gercredit1_valid$V21)
  acc[i] <- sum(diag(conf)) / sum(conf)
  A.U.C[i] <- auc(gercredit1_valid$V21,bin_pred)
}
