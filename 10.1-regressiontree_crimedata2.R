#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#set libraries
library(caret)
library(rpart)
library(plyr)
library(rpart.plot)
library(DAAG)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime
#str(uscrime1)

#test data
testdat <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                      NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)

testdat1=testdat
#add a column for result
testdat1$Crime=0

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#rpart
#grow the tree
split <- createDataPartition(y=uscrime1$Crime, p=0.7, list=FALSE)
usc1_tr <- uscrime1[split,]
usc1_tst <- uscrime1[-split,]

mod1 <- rpart(Crime~.,method="anova", data=usc1_tr)
summary(mod1)
rpart.plot(mod1, extra=101)

mod1_pr<- prune(mod1, cp=mod1$cptable[which.min(mod1$cptable[,"xerror"]),"CP"])
summary(mod1_pr)
rpart.plot(mod1_pr, extra=101)

pred_mod10 <- predict(mod1,usc1_tr)
pred_mod10_pr <- predict(mod1_pr,usc1_tr)

SSE = sum((pred_mod10 - usc1_tr[,16])^2)
SStot = sum((usc1_tr[,16] - mean(usc1_tr[,16]))^2)
Rsq = 1 - SSE/SStot #0.559
Rsq_adj  = Rsq - (1-Rsq)*15/(nrow(usc1_tr)-15-1) #0.211

SSEp = sum((pred_mod10_pr - usc1_tr[,16])^2)
SStotp = sum((usc1_tr[,16] - mean(usc1_tr[,16]))^2)
Rsqp = 1 - SSEp/SStotp # 0.559
Rsq_adjp  = Rsqp - (1-Rsqp)*15/(nrow(usc1_tr)-15-1) #0.211

test_mod1 <- predict(mod1,testdat1) #1380

#pred_model11 <- predict(model1,testdat1)
#pred_modelp11 <- predict(pmodel1,testdat1)

uscrime1_l=uscrime1[which(uscrime1$Po1 <11),]
uscrime1_r=uscrime1[which(uscrime1$Po1 >=11),]
nrow(uscrime1_l)
nrow(uscrime1_r)

##---right node PCA
uscrime1_r.pca <- prcomp(uscrime1_r[,1:15],scale.=TRUE) #,center=TRUE)
summary(uscrime1_r.pca)
screeplot(uscrime1_r.pca,type="lines",col="blue")
#regression on first 4 PCs
PCs <- uscrime1_r.pca$x[,1:4]
PCcrime <- cbind(PCs,uscrime1_r[,16])
PCcrime <- as.data.frame(PCcrime)
summary(PCcrime)
#Crime <- PCcrime$V6
mod3 <- lm(V5~PC1+PC2+PC3+PC4,data = PCcrime)
summary(mod3)
mod3$coefficients

betas <- mod3$coefficients[2:5]
beta0 <- mod3$coefficients[1]
alphas <- uscrime1_r.pca$rotation[,1:4] %*% betas
originalalphas = alphas/sapply(uscrime1_r[,1:15],sd)
originalBeta0 = beta0 - sum(alphas*sapply(uscrime1_r[,1:15],mean)/sapply(uscrime1_r[,1:15],sd))
estimatesuscrime = as.matrix(uscrime1_r[,1:15]) %*% originalalphas + originalBeta0
SSE = sum((estimatesuscrime - uscrime1_r[,16])^2)
SStot = sum((uscrime1_r[,16] - mean(uscrime1_r[,16]))^2)
Rsq = 1 - SSE/SStot #  0.773
Rsq_adj  = Rsq - (1-Rsq)*4/(nrow(uscrime1_r)-4-1) # 0.546
predictcrime = as.matrix(testdat[,1:15]) %*% originalalphas + originalBeta0 #2026

### RIGHT NODE lm()
mod4 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,data = uscrime1_r)
summary(mod4)
mod4_cv <- cv.lm(uscrime1_r,mod4,m=10)
summary(mod4_cv)
test_mod4 <- predict(mod4,testdat1) #1595
SStot0 <- sum((uscrime1_r$Crime - mean(uscrime1_r$Crime))^2)
SSres_mod4 <- sum(mod4$residuals^2)
r2_mod4 <- 1 - SSres_mod4/SStot0 
adjr2_mod4 <- 1 - ((1 - r2_mod4)*(nrow(uscrime1_r) - 1) / (nrow(uscrime1_r)-6-1))
SSres_mod4_cv <- attr(mod4_cv,"ms")*nrow(uscrime1_r)
r2_model0_cv <- 1 - SSres_mod4_cv/SStot0

####left node PCA
uscrime1_l.pca <- prcomp(uscrime1_l[,1:15],scale.=TRUE) #,center=TRUE)
summary(uscrime1_l.pca)
screeplot(uscrime1_l.pca,type="lines",col="blue")
#taking 3PCAs
#regression on first 5 PCs
PCs <- uscrime1_l.pca$x[,1:3]
PCcrime <- cbind(PCs,uscrime1_l[,16])
PCcrime <- as.data.frame(PCcrime)
summary(PCcrime)
mod2 <- lm(V4~PC1+PC2+PC3,data = PCcrime)
summary(mod2)
mod2$coefficients
betas <- mod2$coefficients[2:4]
beta0 <- mod2$coefficients[1]
alphas <- uscrime1_l.pca$rotation[,1:3] %*% betas
originalalphas = alphas/sapply(uscrime1_l[,1:15],sd)
originalBeta0 = beta0 - sum(alphas*sapply(uscrime1_l[,1:15],mean)/sapply(uscrime1_l[,1:15],sd))
estimatesuscrime = as.matrix(uscrime1_l[,1:15]) %*% originalalphas + originalBeta0
SSE = sum((estimatesuscrime - uscrime1_l[,16])^2)
SStot = sum((uscrime1_l[,16] - mean(uscrime1_l[,16]))^2)
Rsq = 1 - SSE/SStot #0.218
Rsq_adj  = Rsq - (1-Rsq)*3/(nrow(uscrime1_l)-3-1)#0.149
predictcrime = as.matrix(testdat[,1:15]) %*% originalalphas + originalBeta0

### LEFT NODE lm()
mod5 <- lm(Crime ~ .,data = uscrime1_l)
summary(mod5)

mod6<- lm(Crime ~ M + Ed + U2 + Ineq + Prob + Time,data = uscrime1_l)
summary(mod6)
mod6_cv <- cv.lm(uscrime1_l,mod6,m=10)
summary(mod6_cv)
test_mod6 <- predict(mod4,testdat1) #1427
SStot0 <- sum((uscrime1_l$Crime - mean(uscrime1_l$Crime))^2)
SSres_mod6 <- sum(mod6$residuals^2)
r2_mod6 <- 1 - SSres_mod6/SStot0 #0.562
adjr2_mod6 <- 1 - ((1 - r2_mod6)*(nrow(uscrime1_l) - 1) / (nrow(uscrime1_l)-6-1)) #0.477
SSres_mod6_cv <- attr(mod6_cv,"ms")*nrow(uscrime1_l)
r2_mod6_cv <- 1 - SSres_mod6_cv/SStot0

#cross validation
library(plyr)
library(rpart)
#set.seed(123)
#form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
folds <- split(uscrime1, cut(sample(1:nrow(uscrime1)),10))
#errs <- rep(NA, length(folds))

for (i in 1:length(uscrime1)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- rpart(Crime~., train, method = "anova")
  tmp.predict <- predict(tmp.model, newdata = test, type = "matrix")
  #conf.mat <- table(test$Species, tmp.predict)
  #errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
  SSE = sum((tmp.model - uscrime1[,16])^2)
  SStot = sum((tmp.model[,16] - mean(uscrime1[,16]))^2)
}
#print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))

#model 2
split <- createDataPartition(y=uscrime1$Crime, p=0.5, list=FALSE)
usc1_tr <- uscrime1[split,]
usc1_tst <- uscrime1[-split,]

mod2 <- rpart(Crime~.,method="anova", data=usc1_tr,control=rpart.control(minsplit=10, cp=0.05))
printcp(mod2) # display the results 
par(mar=c(4,4,4,4))
plotcp(mod2) # visualize cross-validation results 
summary(mod2) # detailed summary of splits
plot(mod2, uniform=TRUE,margin=0.2,main="Classification Tree");text(mod2, use.n=TRUE, all=TRUE, cex=.8)

pred_mod20 <- predict(mod2,usc1_tr)
#pred_mod10_pr <- predict(mod1_pr,uscrime1)

SSE2 = sum((pred_mod20 - usc1_tr[,16])^2)
SStot2 = sum((usc1_tr[,16] - mean(usc1_tr[,16]))^2)
Rsq2 = 1 - SSE2/SStot2 #0.8878385
Rsq_adj2  = Rsq2 - (1-Rsq2)*15/(nrow(usc1_tr)-15-1) #0.8335668

pred_mod21 <- predict(mod2,usc1_tst)
SSE21 = sum((pred_mod21 - usc1_tst[,16])^2)
SStot21 = sum((usc1_tst[,16] - mean(usc1_tst[,16]))^2)
Rsq21 = 1 - SSE21/SStot21 #0.8850162
Rsq_adj21  = Rsq21 - (1-Rsq21)*15/(nrow(usc1_tst)-15-1) #0.6386222

##################################
#using the tree package
#install.packages(tree)
library(caret)
split <- createDataPartition(y=uscrime1$Crime, p=0.7, list=FALSE)
usc1_tr <- uscrime1[split,]
usc1_tst <- uscrime1[-split,]

library(tree)
library(rpart.plot)
modtr1 = tree(usc1_tr$Crime ~ ., data=usc1_tr)
#tree(crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data=uscrime1)
summary(modtr1)
par(mar=c(4,4,4,4))
plot(modtr1); text(modtr1,pretty=0)
rpart.plot(modtr1)

modtr1$frame
modtr1$where
nrow(usc1_tr)
head(usc1_tr)
data1=usc1_tr[which(usc1_tr$Po1 <= 7.5 & usc1_tr$M < 14.75),]
data1=usc1_tr[which(modtr1$where ),]

pred_modtr1 <- predict(modtr1,usc1_tr)
SSE = sum((pred_modtr1 - usc1_tr[,16])^2)
SStot = sum((usc1_tr[,16] - mean(usc1_tr[,16]))^2)
Rsq = 1 - SSE/SStot #0.555
Rsq_adj  = Rsq - (1-Rsq)*15/(nrow(usc1_tr)-15-1) #0.203
test_modtr1 <- predict(modtr1,testdat1) #1339

pred_modtr1 <- predict(modtr1,usc1_tst)
SSE = sum((pred_modtr1 - usc1_tst[,16])^2)
SStot = sum((usc1_tst[,16] - mean(usc1_tst[,16]))^2)
Rsq = 1 - SSE/SStot #0.5
Rsq_adj  = Rsq - (1-Rsq)*15/(nrow(usc1_tst)-15-1) #2.37


#cross validation - Cross validate to see whether pruning the tree will improve performance
modtr1.cv = cv.tree(modtr1)
plot(modtr1.cv)
modtr1.cv$size
modtr1.cv$dev
modtr1.cv$k

# prune the tree where the deviance plateaus at 2.5
pmodtr1 <- prune(modtr1, best=3)
plot(pmodtr1);text(pmodtr1,pretty=0)

#predict test data
pred_pmodtr1 <- predict(pmodtr1,usc1_tst)
plot(pred_pmodtr1,usc1_tst$Crime)
abline(0,1)
summary(pmodtr1)
pmodtr1$where
pmodtr1$xlevels$Po1
pmodtr1$weights
pmodtr1$frame
attributes(pmodtr1)

#verify SSE & others for predictions
SSE_pmodtr1 = sum((pred_pmodtr1 - usc1_tst[,16])^2)
SStot_pmodtr1 = sum((usc1_tst[,16] - mean(usc1_tst[,16]))^2)
Rsq_pmodtr1 = 1 - SSE_pmodtr1/SStot_pmodtr1;print (Rsq_pmodtr1) #
Rsq_adj_pmodtr1  = Rsq_pmodtr1 - (1-Rsq_pmodtr1)*15/(nrow(usc1_tst)-15-1);print(Rsq_adj_pmodtr1) #

data1=usc1_tr[which(pmodtr1$where == 3),]
data1=usc1_tr[which(pmodtr1$where == 3),]
mean((pred_pmodtr1 - usc1_tst$Crime)^2) #135763.5
pred_modeltr10 <- predict(modeltr1,testdat1)

#deviance values large
#we are using all data, reducing by pruning will reducing R squared

##################################################
#randomforest
library(randomForest)
samp <- sample(nrow(uscrime), 0.6 * nrow(uscrime))
train.usc <- uscrime[samp, ]
test.usc <- uscrime[-samp, ]
modrf1 <- randomForest(Crime ~ ., data=uscrime1, subset = samp)
summary(modrf1)
mean(modrf1$rsq)
plot(modrf1, type="l")
pred_rf1 = predict(modrf1)
sse_rf1= sum((pred_rf1 - train.usc$Crime)^2)
sst_rf1 = sum((train.usc$Crime- mean(train.usc$Crime))^2)
rsq_rf1<-1-sse_rf1/sst_rf1 #0.52

modrf2 <- randomForest(Crime ~ ., data=uscrime1,mtry=5,ntree=1000,importance=TRUE, subset = samp)
summary(modrf2)
pred_rf2 = predict(modrf2)
sse_rf2= sum((pred_rf2 - train.usc$Crime)^2)
sst_rf2 = sum((train.usc$Crime- mean(train.usc$Crime))^2)
rsq_rf2<-1-sse_rf2/sst_rf2 #0.55

pred_modrf1 <- predict(modrf1,testdat1) # 1283
pred_modrf2 <- predict(modrf2,testdat1) # 1304

######## LEAVE ONE OUT CROSS VALIDATION ###

sse_cv=0
for (i in 1:nrow(uscrime1))  {
  temp=randomForest(Crime~.,data = uscrime1[-i,],mtry=5)
  sse_cv = sse_cv + (predict(temp,uscrime1[i,])-uscrime1[i,16])^2
}

sst_cv = sum((uscrime1$Crime- mean(uscrime1$Crime))^2)
rsq_ssecv= 1 - sse_cv / sst_cv  # 0.422
