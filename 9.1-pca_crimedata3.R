#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

par(mar=c(1,1,1,1))

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

pairs(uscrime)

#running the pca
#scale = True, center = True
uscrime.pca <- prcomp(uscrime[,1:15],scale.=TRUE,center=TRUE)
print(uscrime.pca)
summary(uscrime.pca)
attributes(uscrime.pca)
uscrime.pca$rotation
summary(uscrime.pca$rotation)
attributes(uscrime.pca$rotation)
uscrime.pca$center
uscrime.pca$scale
uscrime.pca$sdev
uscrime.pca$x
attributes(uscrime.pca$x)

screeplot(uscrime.pca,type="lines",col="blue")

biplot(uscrime.pca,scale=0, cex=.7)

library(psych)
pairs.panels(uscrime.pca$x,gap=0)

var <- uscrime.pca$sdev^2
propvar <- var/sum(var)
plot(propvar,xlab="Principal Component",ylab = "Proportion of Variance Explained")

#regression on first 5 PCs
PCs <- uscrime.pca$x[,1:5]
PCcrime <- cbind(PCs,uscrime[,16])
PCcrime <- as.data.frame(PCcrime)
summary(PCcrime)

PCs1 <- uscrime.pca$x[,c(1,2,4,5)]
PCcrime1 <- cbind(PCs1,uscrime[,16])
PCcrime1 <- as.data.frame(PCcrime1)
summary(PCcrime1)

#linear regression models
Crime <- PCcrime$V6
model <- lm(Crime~PC1+PC2+PC3+PC4+PC5,data = PCcrime)
summary(model)
model$coefficients
par(mfrow = c(2, 2))
plot(model)

#linear regression models
#dropped PC3
model1 <- lm(Crime~PC1+PC2+PC4+PC5,data = PCcrime1)
summary(model1)
model1$coefficients
par(mfrow = c(2, 2))
plot(model1)

# getting the original coeffs
betas <- model$coefficients[2:6]
beta0 <- model$coefficients[1]

betas1 <- model1$coefficients[2:5]
beta10 <- model1$coefficients[1]

#unscale & uncenter, transform to original factors
alphas <- uscrime.pca$rotation[,1:5] %*% betas
alphas1 <- uscrime.pca$rotation[,c(1,2,4,5)] %*% betas1

originalalphas = alphas/sapply(uscrime[,1:15],sd)
originalBeta0 = beta0 - sum(alphas*sapply(uscrime[,1:15],mean)/sapply(uscrime[,1:15],sd))

originalalphas1 = alphas1/sapply(uscrime[,1:15],sd)
originalBeta10 = beta10 - sum(alphas1*sapply(uscrime[,1:15],mean)/sapply(uscrime[,1:15],sd))

#calculate R square & Adjusted R square
estimatesuscrime = as.matrix(uscrime[,1:15]) %*% originalalphas + originalBeta0

estimatesuscrime1 = as.matrix(uscrime[,1:15]) %*% originalalphas1 + originalBeta10

SSE = sum((estimatesuscrime - uscrime[,16])^2)
SStot = sum((uscrime[,16] - mean(uscrime[,16]))^2)

SSE1 = sum((estimatesuscrime1 - uscrime[,16])^2)
SStot1 = sum((uscrime[,16] - mean(uscrime[,16]))^2)

Rsq = 1 - SSE/SStot #0.645
Rsq_adj  = Rsq - (1-Rsq)*5/(nrow(uscrime)-5-1) #0.602

Rsq1 = 1 - SSE1/SStot1 #0.637
Rsq_adj1  = Rsq - (1-Rsq1)*4/(nrow(uscrime)-4-1) #0.611

#test data
testdat <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                      NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)
#predict test crime
predictcrime = as.matrix(testdat[,1:15]) %*% originalalphas + originalBeta0

predictcrime1 = as.matrix(testdat[,1:15]) %*% originalalphas1 + originalBeta10


#cross validation

library(DAAG)
#5 fold validation
model_cv <- cv.lm(PCcrime,model,m=5)
summary(model_cv)
model1_cv <- cv.lm(PCcrime1,model1,m=5)

AIC(model)
AIC(model1)
