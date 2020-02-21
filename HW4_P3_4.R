rm(list=ls())
setwd("C:\\RProgs")
set.seed(3)

install.packages('R.matlab')
install.packages('fda')
install.packages('pracma')
install.packages('gglasso')

library(R.matlab)
library(fda)
library(pracma)
library(gglasso)

data=readMat('NSC.mat')

py = 1 # for y #10 for x     #Number of parameters
px = 10
tp = 4     #Number of true parameters
n = 203     #Lenght of observations
m = 150    #Number of observaations
spb = 8    #knots

par(mfrow=c(2,5))
for(i in 1:px)
{
  matplot(t(data$x[[i]][[1]]), type = "l", xlab = i,ylab = "")
}

#b spline
splinebasis_B=create.bspline.basis(c(0,1),spb)
x = seq(0,1,length=n)
size(x)
base_B=eval.basis(as.vector(x),splinebasis_B)
P = t(base_B)
size(base_B)
size(P)

# reduce dimensionality
#predictors
X = array(dim=c(m,n,px))
size(X)
for(i in 1:px)
{
  X[,,i] = data$x[[i]][[1]] #x_1[[i]]
}
size(X)
Z = array(dim=c(dim(X)[1],spb,px))
for(i in 1:px)
{
  Z[,,i] = X[,,i]%*%base_B/n 
}
size(Z)
Zx = matrix(Z,m,spb*px) #stack the predictors
size(Zx)

#output
Y = array(dim=c(m,n,py))
size(Y)
for(i in 1:py)
{
  Y[,,i] = data$y
}
size(Y)
Zy = array(dim=c(dim(Y)[1],spb,py))
size(Zy)
for(i in 1:py)
{
  Zy[,,i] = Y[,,i]%*%base_B/n 
}
size(Zy)
#Zy1 = matrix(Zy,m ,spb * py)
Zy1 = matrix(Zy,1 , m * spb * py)
Zy1[1:150]
size(Zy1)

#regression group lasso
group = rep(1:px,each=spb)
glasso = cv.gglasso(Zx,Zy1,group,loss = "ls")
coef(glasso,s="lambda.1se")[2:(m+1)]
lambda = glasso$lambda.min
coef = matrix(coef(glasso,s="lambda.1se")[2:(m+1)],spb,px)
View(coef)
coef = base_B%*%coef
matplot(x,coef,type="l",lwd=5)
matplot(x,coef,col=c(5,1,6,2,7,3,8,4,9,10),lty=rep(1,10),type="l",ylim=c(-20,20),lwd=5)
legend(0,20,c(1:10),col=c(5,1,6,2,7,3,8,4,9,10),lty = rep(1,10),lwd=5,)
