rm(list=ls())
setwd("C:\\RProgs")
set.seed(3)

library(R.matlab)
library(fda)
library(pracma)
library(gglasso)

data=readMat('NSC.mat')

py = 1 # for y #10 for x     #Number of parameters
px = 10
tp = 4     #Number of true parameters
n = 203     #Length of observations
m = 150    #Number of observaations
spb = 8    #knots
mr = 5

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
Zx = matrix(Z,m, spb*px) #stack the predictors
size(Zx)

#output
Y = array(dim=c(m,n))
size(Y)
Y=data$y
size(Y)
Zy = array(dim=c(dim(Y)[1],spb))
size(Zy)
Zy = Y%*%base_B/n 
size(Zy)


Zx_r1=matrix(Zx[1:mr,],mr,80)
size(Zx_r1)
Zy_r1=Zy[1:mr,]
size(Zy_r1)


#load test data
mt=50
datat=readMat('NSC.test.mat')

#decompose test predictors
Xt = array(dim=c(mt,n,px))
size(Xt)
for(i in 1:px)
{
  Xt[,,i] = datat$x[[i]][[1]] #x_1[[i]]
}
size(Xt)
Zt = array(dim=c(dim(Xt)[1],spb,px))
for(i in 1:px)
{
  Zt[,,i] = Xt[,,i]%*%base_B/n 
}
size(Zt)
Ztx = matrix(Zt,mt, spb*px) #stack the predictors
size(Ztx)

#decompose test output
Yt = array(dim=c(m,n))
size(Yt)
Yt=datat$y
size(Yt)
Zty = array(dim=c(dim(Yt)[1],spb))
size(Zty)
Zty = Yt%*%base_B/n 
size(Zty)

group = rep(1:px,each=spb); group
lambdas=array(dim=c(8,1)); size(lambdas)
lambdas1se=array(dim=c(8,1)); size(lambdas1se)
coefs=array(dim=c(8,10,10));size(coefs)
yglassos=array(dim=c(50,1,10));size(yglassos)
mse_glassos=array(dim=c(8,1)); size(mse_glassos)

#regression group lasso
for (j in 2:3)
{
  fprintf("iteration :- %d \n",j)
  
  glasso = cv.gglasso(Zx_r1,Zy_r1[,j],group,loss = "ls")
  lambdas[j] = glasso$lambda.min; lambdas[j]
  lambdas1se[j] = glasso$lambda.1se
  coefs[,,j] = matrix(coef(glasso,s="lambda.1se")[2:(80+1)],spb,px)
  
  #predict & mse
  yglassos[,,j]=predict(glasso, Ztx, s = lambdas[j])
  mse_glassos[j] = sum((Zty[,j]-yglassos[,,j])^2)/50; 
  
  #write.table(glasso, file = sprintf("mtcars%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(lambdas[j], file = sprintf("lambdas%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(lambdas1se[j], file = sprintf("lambdas1se%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(coefs[,,j], file = sprintf("coefs%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(mse_glassos[j], file = sprintf("mse_glassos%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(yglassos[,,j], file = sprintf("yglassos%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
}
