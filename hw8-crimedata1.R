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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"
summary(usc1_scld)

# 5 fold CV
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5)
lmFit_Step=train(Crime ~ ., data = usc1_scld, "lmStepAIC", scope = list(lower=Crime~1, upper=Crime~.) , direction="backward",trControl=ctrl)
summary(lmFit_Step)
#plot(lmFit_Step)
lmFit_Step$finalModel

#creating a regression model with the 8 predictors identfieid by CV
mod_Step=lm(Crime~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data=usc1_scld)
summary(mod_Step)
AIC(mod_Step)
#R2_mod 0.667621

mod_Step=lm(Crime~ M + Ed + Po1 + U1 + U2 + Ineq + Prob, data=usc1_scld)
summary(mod_Step)
#R2_mod 0.6622135

mod_Step=lm(Crime~ M + Ed + Po1 + U2 + Ineq + Prob, data=usc1_scld)
summary(mod_Step)
#plot(mod_Step)


# R2_mod 0.6661638

#cross validation
SStot <- sum((usc1_scld$Crime - mean(usc1_scld$Crime))^2)
totsse <- 0

for(i in 1:nrow(usc1_scld)) {
  #mod_Step_i = lm(Crime~ M + Ed + Po1 + U2 + Ineq + Prob, data = usc1_scld[-i,])  ##Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1
  #mod_Step_i=lm(Crime~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data=usc1_scld[-i,])
  #mod_Step_i=lm(Crime~ M + Ed + Po1 + U1 + U2 + Ineq + Prob, data=usc1_scld[-i,])  
  mod_Step_i=lm(Crime~ M + Ed + Po1 + U2 + Ineq + Prob, data=usc1_scld[-i,])
  pred_i <- predict(mod_Step_i, newdata=usc1_scld[i,])
  totsse <- totsse + ((pred_i - usc1_scld[i,16])^2)
}

R2_mod <- 1 - totsse/SStot
R2_mod
plot(mod_Step_i)


mod_Step = lm(Crime ~ Prob+U2+M+Ed+Ineq+Po1, data = usc1_scld)
summary(mod_Step)

mod_Step = lm(Crime ~ U1+Prob+U2+M+Ed+Ineq+Po1, data = usc1_scld)
summary(mod_Step)
AIC(mod_Step)

# STEP FUNCTION
fit <- lm(Crime~.,data=usc1_scld)
step(fit, direction="backward")

################################################
#-------------lasso regression------------------#
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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"
summary(usc1_scld)

XP=data.matrix(usc1_scld[,-16])
YP=data.matrix(usc1_scld$Crime)
lasso=cv.glmnet(x=as.matrix(usc1_scld[,-16]),y=as.matrix(usc1_scld$Crime),alpha=1, nfolds=5, type.measure="mse",family="gaussian")
coef(lasso, s=lasso$lambda.min)
plot(lasso)

mod_lasso = lm(Crime ~So+M+Ed+Po1+LF+M.F+NW+U1+U2+Wealth+Ineq+Prob, data = usc1_scld)
summary(mod_lasso)
AIC(mod_lasso)

mod1_lasso = lm(Crime ~M+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob, data = usc1_scld)
summary(mod1_lasso)
AIC(mod1_lasso)

mod2_lasso = lm(Crime ~M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = usc1_scld)
summary(mod2_lasso)
AIC(mod2_lasso)

mod3_lasso = lm(Crime ~M+Ed+Po1+U2+Ineq+Prob, data = usc1_scld)
summary(mod3_lasso)
AIC(mod3_lasso)

#alpha 0

XP=data.matrix(usc1_scld[,-16])
YP=data.matrix(usc1_scld$Crime)
lasso=cv.glmnet(x=as.matrix(usc1_scld[,-16]),y=as.matrix(usc1_scld$Crime),alpha=0, type.measure="mse",family="gaussian")
coef(lasso, s=lasso$lambda.min)
plot(lasso)


#cross validation
SStot <- sum((usc1_scld$Crime - mean(usc1_scld$Crime))^2)
totsse <- 0

for(i in 1:nrow(usc1_scld)) {
  #mod_Step_i=lm(Crime~ M + Ed + Po1 + U2 + Ineq + Prob, data=usc1_scld[-i,])
  mod_Step_i=lm(Crime~ So+M+Ed+Po1+LF+M.F+NW+U1+U2+Wealth+Ineq+Prob, data=usc1_scld[-i,])
  pred_i <- predict(mod_Step_i, newdata=usc1_scld[i,])
  totsse <- totsse + ((pred_i - usc1_scld[i,16])^2)
}

R2_mod <- 1 - totsse/SStot
R2_mod
plot(mod_Step_i)

######################################################
#--------- elastic net -----#
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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"
summary(usc1_scld)

R2=c()
for (i in 0:10) {
  mod_elastic = cv.glmnet(x=as.matrix(usc1_scld[,-16]),y=as.matrix(usc1_scld$Crime),
                          alpha=i/10,nfolds=5,type.measure = "mse",family="gaussian")
  R2 = cbind(R2, mod_elastic$glmnet.fit$dev.ratio[which(mod_elastic$glmnet.fit$lambda == mod_elastic$lambda.min)])
}
R2
#plot(mod_elastic)

alpha_best = (which.max(R2)-1)/10
alpha_best

Elastic_net=cv.glmnet(x=as.matrix(usc1_scld[,-16]),y=as.matrix(usc1_scld$Crime),alpha=alpha_best,
                      nfolds = 5,type.measure="mse",family="gaussian")

coef(Elastic_net, s=Elastic_net$lambda.min)

mod3_elast = lm(Crime ~So + M + Ed + Po1+ M.F + Pop + NW + U1 + U2+ Wealth + Ineq + Prob, data = usc1_scld)
summary(mod3_elast)
AIC(mod3_elast)

#cross validation
SStot <- sum((usc1_scld$Crime - mean(usc1_scld$Crime))^2)
totsse <- 0

for(i in 1:nrow(usc1_scld)) {
  #mod_Step_i=lm(Crime~ So + M + Ed + Po1+ M.F + Pop + NW + U1 + U2+ Wealth + Ineq + Prob, data=usc1_scld[-i,])
  mod_Step_i=lm(Crime~ So + M + Ed + Po1+ Po2+M.F + NW + U2+ Ineq + Prob, data=usc1_scld[-i,])
  pred_i <- predict(mod_Step_i, newdata=usc1_scld[i,])
  totsse <- totsse + ((pred_i - usc1_scld[i,16])^2)
}

R2_mod <- 1 - totsse/SStot
R2_mod

##############################
#-----------PCA---------------#
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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"
summary(usc1_scld)

pca <- prcomp(uscrime1[,1:15], scale=TRUE)
summary(pca)
#screeplot(pca,type="lines")

PCcrime <- as.data.frame(cbind(pca$x, uscrime1[,16]))
colnames(PCcrime)[16] <- "Crime"

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

lmFit_Step_PC <- train(Crime ~ ., data = PCcrime, "lmStepAIC", scope = 
              list(lower = Crime~1, upper = Crime~.), 
              direction = "backward",trControl=ctrl)

mod_Step_PC = lm(Crime ~ PC15+PC6+PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime)
summary(mod_Step_PC)
AIC(mod_Step_PC)

# removed PC15 and PC6
SStot = sum((uscrime1$Crime - mean(uscrime1$Crime))^2)
totsse = 0

for(i in 1:nrow(PCcrime)) {
  #mod_lasso_i = lm(Crime ~ PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime[-i,])
  mod_lasso_i = lm(Crime ~ PC15+PC6+PC14+PC7+PC4+PC12+PC2+PC1+PC5, data = PCcrime[-i,])
  pred_i = predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse = totsse + ((pred_i - PCcrime[i,16])^2)
}
R2_mod = 1 - totsse/SStot
R2_mod

#####################################
#---lasso---#
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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"

pca <- prcomp(uscrime1[,1:15], scale=TRUE)

PCcrime <- as.data.frame(cbind(pca$x, uscrime1[,16]))
colnames(PCcrime)[16] <- "Crime"

XP=data.matrix(PCcrime[,-16])
YP=data.matrix(PCcrime$Crime)
lasso_PC=cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),alpha=1
                   ,type.measure="mse",family="gaussian")

coef(lasso_PC, s=lasso_PC$lambda.min)

mod_lasso_PC = lm(Crime ~ ., data = PCcrime)
summary(mod_lasso_PC)
AIC(mod_lasso_PC)

SStot = sum((uscrime1$Crime - mean(uscrime1$Crime))^2)
totsse = 0

for(i in 1:nrow(PCcrime)) {
  #mod_lasso_i = lm(Crime ~ PC1+PC2+PC4+PC5+PC7+PC12+PC14, data = PCcrime[-i,])
  mod_lasso_i = lm(Crime ~ ., data = PCcrime[-i,])
  pred_i = predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse = totsse + ((pred_i - PCcrime[i,16])^2)
}

R2_mod = 1 - totsse/SStot
R2_mod

############################################
#---- elastic net ----#

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
library(glmnet)
library(MASS)
library(ggplot2)
library(gridExtra)

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#scaling data
usc1_scld=as.data.frame(scale(uscrime1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
usc1_scld=cbind(uscrime1[,2],usc1_scld,uscrime1[,16])
colnames(usc1_scld)[1]="So"
colnames(usc1_scld)[16]="Crime"

pca <- prcomp(uscrime1[,1:15], scale=TRUE)

PCcrime <- as.data.frame(cbind(pca$x, uscrime1[,16]))
colnames(PCcrime)[16] <- "Crime"

R2_PC=c()

for (i in 0:10) {
  model = cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),
                    alpha=i/10,nfolds = 5,type.measure="mse",family="gaussian")
  R2_PC = cbind(R2_PC,model$glmnet.fit$dev.ratio[which(model$glmnet.fit$lambda == model$lambda.min)])
}

R2_PC

alpha_best_PC = (which.max(R2_PC)-1)/10
alpha_best_PC


Elastic_net_PC=cv.glmnet(x=as.matrix(PCcrime[,-16]),y=as.matrix(PCcrime$Crime),alpha=alpha_best_PC,
                         nfolds = 5,type.measure="mse",family="gaussian")

coef(Elastic_net_PC, s=Elastic_net_PC$lambda.min)

mod_Elastic_net_PC = lm(Crime ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC10+PC12+PC14+PC15, data = PCcrime)
summary(mod_Elastic_net_PC)
AIC(mod_Elastic_net_PC)

SStot = sum((uscrime1$Crime - mean(uscrime1$Crime))^2)
totsse = 0

for(i in 1:nrow(PCcrime)) {
  #mod_lasso_i = lm(Crime ~ PC1+PC2+PC4+PC5+PC7+PC12+PC14, data = PCcrime[-i,])
  mod_lasso_i = lm(Crime ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC10+PC12+PC14+PC15, data = PCcrime[-i,])
  pred_i = predict(mod_lasso_i,newdata=PCcrime[i,])
  totsse = totsse + ((pred_i - PCcrime[i,16])^2)
}

R2_mod = 1 - totsse/SStot
R2_mod
