#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#par(mar=c(1,1,1,1))

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)


# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

pairs(uscrime)

#running the pca
#scale = True, center = True
uscrime.pca <- prcomp(uscrime[,1:15],scale.=TRUE) #,center=TRUE)
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


#require(graphics)
screeplot(uscrime.pca,type="lines",col="blue")

# pcaCharts(uscrime.pca)

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


#linear regression models
Crime <- PCcrime$V6
model <- lm(Crime~PC1+PC2+PC3+PC4+PC5,data = PCcrime)
summary(model)
model$coefficients

par(mfrow = c(2, 2))
plot(model)

betas <- model$coefficients[2:6]
beta0 <- model$coefficients[1]

alphas <- uscrime.pca$rotation[,1:5] %*% betas

#sigma = uscrime.pca$scale
#mu = uscrime.pca$center

#alphas_original = alphas%*%(1/sigma)
#alpha0_orginal = beta0 - sum(mu/sigma)

originalalphas = alphas/sapply(uscrime[,1:15],sd)
originalBeta0 = beta0 - sum(alphas*sapply(uscrime[,1:15],mean)/sapply(uscrime[,1:15],sd))

estimatesuscrime = as.matrix(uscrime[,1:15]) %*% originalalphas + originalBeta0

SSE = sum((estimatesuscrime - uscrime[,16])^2)
SStot = sum((uscrime[,16] - mean(uscrime[,16]))^2)

Rsq = 1 - SSE/SStot
Rsq_adj  = Rsq - (1-Rsq)*5/(nrow(uscrime)-5-1)

#alpha <- uscrime.pca$rotation[,1:5] %*% beta
#t(alphas)

#You can multiply the principal components matrix pca$x by t(pca$rotation) .
#x_scaled <- uscrime.pca$x %*% t(uscrime.pca$rotation)

#That will give the original data scaled.
#You can unscale it:
#x_orig <- scale(x_scaled, center=FALSE, scale=1/uscrime.pca$scale)
#x_orig <- scale(x_orig, center=-1*uscrime.pca$center, scale=FALSE)

#test data
testdat <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                      NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)

#testdat.pca <- prcomp(testdat[,1:15]) #,center=TRUE)

#x_orig <- as.data.frame(x_orig)
#x_orig[,scaled:scale] * testdat$M

#PCcrimetst <- cbind(PCs,testdat[,16])
#PCcrime <- as.data.frame(PCcrimetst)

#pred_model <- predict(model,as.data.frame(testdat.pca))
#155.4349

predictcrime = as.matrix(testdat[,1:15]) %*% originalalphas + originalBeta0





