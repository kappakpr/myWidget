rm(list=ls())
setwd("C:\\RProgs")

##Reading the data: 
  data = read.csv("GA_EDVisits.csv",header=TRUE) 
  nrow(data)
  summary(data)
  data = na.omit(data)
  summary(data)
  nrow(data)
  # Get names of the column 
  names = colnames(data)
  attach(data)
  
  ## standardized predictors 
  sAvgDistS = scale(log(SpecDist)) 
  sAvgDistP = scale(log(PedDist))
  sMedianIncome = scale(MedianIncome)
  sNumHospitals = scale(No.Hospitals)
  sPercentLessHS = scale(PercentLessHS)
  sPercentHS = scale(PercentHS)
  
  Y = cbind(ED.visits, Asthma_children-ED.visits)
  head(Y)
  head(data)
  
  ## Define interaction terms
  DistA5.9 = sAvgDistS*A5.9
  DistA10.14 = sAvgDistS* A10.14
  DistIncome = sAvgDistS*sMedianIncome
  DistLessHS = sAvgDistS*sPercentLessHS
  DistHS = sAvgDistS*sPercentHS
  DistPA5.9 = sAvgDistP*A5.9
  DistPA10.14 = sAvgDistP* A10.14
  DistPIncome = sAvgDistP*sMedianIncome
  DistPLessHS = sAvgDistP*sPercentLessHS
  DistPHS = sAvgDistP*sPercentHS
  X = cbind(A5.9, A10.14, sAvgDistS, sAvgDistP, sMedianIncome, sPercentLessHS, sPercentHS, sNumHospitals, DistA5.9, DistA10.14, DistIncome, DistLessHS, DistHS, DistPA5.9, DistPA10.14, DistPIncome, DistPLessHS, DistPHS)

  colnames(X) <- c("A5.9","A10.14","sAvgDistS","sAvgDistP","sMedianIncome","sPercentLessHS","sPercentHS","sNumHospitals","DistA5.9","DistA10.14","DistIncome","DistLessHS","DistHS","DistPA5.9","DistPA10.14","DistPIncome","DistPLessHS","DistPHS")
  
  summary(data)  

  #model1 = lm(ED.visits~County+A5.9+A10.14+A15.17+No.Hospitals+PercentLessHS+PercentHS+MedianIncome+SpecDist+PedDist,data=data)
  #summary(model1)
  
  #model1.1 = lm(ED.visits+Asthma_children ~A5.9+A10.14+A15.17+No.Hospitals+PercentLessHS+PercentHS+SpecDist+PedDist,data=data)
  #summary(model1.1)
  
  model1 = glm(ED.visits~A5.9+A10.14+A15.17+sNumHospitals+sPercentLessHS+sPercentHS+sAvgDistS+sAvgDistP+sMedianIncome,data=data)
  summary(model1)
  
  model2 = glm(ED.visits~A5.9+A10.14+A15.17+sPercentLessHS+sPercentHS+sAvgDistS+sAvgDistP+sMedianIncome+DistA5.9+DistA10.14 + DistIncome + DistLessHS + DistHS + DistPA5.9 + DistPA10.14 + DistPIncome + DistPLessHS + DistPHS,data=data)
  summary(model2)
  
  model2.1 = glm(ED.visits~A5.9+A10.14+A15.17+sNumHospitals+sPercentLessHS+sPercentHS+sAvgDistS+sAvgDistP+sMedianIncome+DistA5.9+DistA10.14 + DistIncome + DistLessHS + DistHS + DistPA5.9 + DistPA10.14 + DistPIncome + DistPLessHS + DistPHS,data=data)
  summary(model2.1)
  
  
  full = model2.1  
  minimum = model1
  
  # Forward
  forward.model = step(minimum, scope = list(lower=minimum, upper = full), direction = "forward")
  summary(forward.model)  
  AIC(forward.model)
  # Backward
  backward.model = step(full, scope = list(lower=minimum, upper = full), direction = "backward")
  summary(backward.model)
  AIC(backward.model)
  # Both
  both.min.model = step(minimum, scope = list(lower=minimum, upper = full), direction = "both")
  summary(both.min.model)
  AIC(both.min.model)
  both.full.model =step(full, scope = list(lower=minimum, upper = full), direction = "both")
  summary(both.full.model)
  AIC(both.full.model)
  
  
  #lasso
  
  ## Apply Lasso /Elastic Net
  library(glmnet)
  predictors1 = cbind(data, sAvgDistS, sAvgDistP, sMedianIncome, sPercentLessHS, sPercentHS, sNumHospitals, DistA5.9, DistA10.14, DistIncome, DistLessHS, DistHS, DistPA5.9, DistPA10.14, DistPIncome, DistPLessHS, DistPHS)
  predictors = data.matrix(X) 
    #data.matrix(predictors1[,-c(1,2,3)])
  ## Lasso Regression
  # Find the optimal lambda using 10-fold CV 
  lassomodel.cv=cv.glmnet(predictors,ED.visits,alpha=1,nfolds=10)
  ## Fit lasso model with 100 values for lambda
  lassomodel = glmnet(predictors,ED.visits, alpha = 1, nlambda = 100)
  ## Plot coefficient paths
  plot(lassomodel,xvar="lambda",lwd=2,label=TRUE)
  abline(v=log(lassomodel.cv$lambda.min),col='black',lty = 2,lwd=2)
  ## Extract coefficients at optimal lambda
  coef(lassomodel,s=lassomodel.cv$lambda.min)

  
  ## Elastic Net Regression
  # Find the optimal lambda using 10-fold CV  
  enetmodel.cv=cv.glmnet(predictors,ED.visits,alpha=0.5,nfolds=10)
  ## Fit lasso model with 100 values for lambda
  enetmodel = glmnet(predictors,ED.visits, alpha = 0.5, nlambda = 100)
  ## Plot coefficient paths
  plot(enetmodel,xvar="lambda",label=T, lwd=2)
  abline(v=log(enetmodel.cv$lambda.min),col='black',lty = 2,lwd=2)
  ## Extract coefficients at optimal lambda
  coef(enetmodel,s=enetmodel.cv$lambda.min)  
  
  
