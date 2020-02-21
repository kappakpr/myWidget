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

for (i in 1:80) {
  Ztx[,i]=standardize(Ztx[,1])
}

#decompose test output
Yt = array(dim=c(mt,n))
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
coefs=array(dim=c(8,10,8));size(coefs)
all_coefs=array(dim=c(81,1,8));size(all_coefs)
yglassos=array(dim=c(50,1,8));size(yglassos)
mse_glassos=array(dim=c(8,1)); size(mse_glassos)

library(psycho)
for (i in 1:80) {
Zx[,i]=standardize(Zx[,1])
}

if (! dir.exists("HW4")) {
  dir.create("HW4")
}

#regression group lasso
for (j in 1:8)
{
  fprintf("iteration :- %d \n",j)
  write.table(j, file = sprintf("HW4\\iteration%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  
  glasso = cv.gglasso(Zx,Zy[,j],group,loss = "ls")
  lambdas[j] = glasso$lambda.min; lambdas[j]
  lambdas1se[j] = glasso$lambda.1se
  coefs[,,j] = matrix(coef(glasso,s="lambda.1se")[2:(80+1)],spb,px)
  all_coefs[,,j] = coef(glasso,s="lambda.1se")

#predict & mse
  yglassos[,,j]=predict(glasso, Ztx, s = lambdas1se[j])
  mse_glassos[j] = sum((Zty[,j]-yglassos[,,j])^2)/mt; 
  
  #write.table(glasso, file = sprintf("mtcars%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(lambdas[j], file = sprintf("HW4\\lambdas%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(lambdas1se[j], file = sprintf("HW4\\lambdas1se%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(coefs[,,j], file = sprintf("HW4\\coefs%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(all_coefs[,,j], file = sprintf("HW4\\all_coefs%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(mse_glassos[j], file = sprintf("HW4\\mse_glassos%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
  write.table(yglassos[,,j], file = sprintf("HW4\\yglassos%d.txt",j), sep = ",", row.names = TRUE, col.names = NA)
}


write.table(lambdas, file = sprintf("HW4\\lambdas.txt",j), sep = ",", row.names = TRUE, col.names = NA)
write.table(lambdas1se, file = sprintf("HW4\\lambdas1se.txt",j), sep = ",", row.names = TRUE, col.names = NA)
write.table(coefs, file = sprintf("HW4\\coefs.txt",j), sep = ",", row.names = TRUE, col.names = NA)
write.table(all_coefs, file = sprintf("HW4\\all_coefs.txt",j), sep = ",", row.names = TRUE, col.names = NA)
write.table(mse_glassos, file = sprintf("HW4\\mse_glassos.txt",j), sep = ",", row.names = TRUE, col.names = NA)
write.table(yglassos, file = sprintf("HW4\\yglassos.txt",j), sep = ",", row.names = TRUE, col.names = NA)


#coefs_all=array(dim=c(8,10,8));size(coefs)