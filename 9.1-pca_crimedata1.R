#Clear environment
rm(list=ls())

#plot function
# pcaCharts <- function(x) {
#   x.var <- x$sdev ^ 2
#   x.pvar <- x.var/sum(x.var)
#   print("proportions of variance:")
#   print(x.pvar)
#   
#   par(mfrow=c(2,2))
#   plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
#   plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
#   screeplot(x)
#   screeplot(x,type="l")
#   par(mfrow=c(1,1))
# }

#set working directory
setwd("C:\\RProgs")

#par(mar=c(1,1,1,1))

#set plot directory
#jpeg(file = "C:\\RProgs\\mycrimedataplots.jpeg")

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)


# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

pairs(uscrime)
cor_matrix = round(cor(uscrime[,-16]),2)

# sapply(uscrime[1:15],var)
# range(sapply(uscrime[1:15],var))
# uscrime.stand <- as.data.frame(scale(uscrime[,1:15]))
# sapply(uscrime.stand,sd) 
# range(sapply(uscrime.stand,sd))

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


#require(graphics)
screeplot(uscrime.pca,type="lines",col="blue")

# pcaCharts(uscrime.pca)

biplot(uscrime.pca,scale=0, cex=.7)

library(psych)
pairs.panels(uscrime.pca$x,gap=0)

#flip rotation code
# pca.out <- uscrime.pca
# pca.out$rotation <- -pca.out$rotation
# pca.out$x <- -pca.out$x
# biplot(pca.out,scale=0, cex=.7)


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
betas <- model$coefficients[2:6]
beta <- model$coefficients[1]


alphas <- uscrime.pca$rotation[,1:5] %*% betas
alphas
t(alphas)

#unscale coeffiencients
# The center and scale are the values you can use, and is used by the 
# prcomp function, to transform the x from scaled to unscaled with the 
# general function original_x = (scaled_x - center)/scale
# original_x = (scaled_x - means)/standard_deviation
# pca$x%*%t(pca$rotation)
#x_orig <- scale(x_scaled, center=FALSE, scale=1/sapply(data[,1:15],sd))
#x_orig <- scale(x, center=-1*sapply(data[,1:15],mean), scale=FALSE)

x_orig <- scale(t(alphas), center=FALSE, scale=1/sapply(uscrime[,1:15],sd))
x_orig <- scale(x_orig, center=-1*sapply(uscrime[,1:15],mean), scale=FALSE)

par(mfrow = c(2, 2))
plot(model)


#test data
testdat <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                      NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)

PCcrimetst <- cbind(PCs,testdat[,16])
PCcrime <- as.data.frame(PCcrimetst)

pred_model <- predict(model,testdat)
#155.4349





