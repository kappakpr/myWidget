dataAdult=read.csv("DataADULT.csv",header=TRUE)
attach(dataAdult)

###############################################################################
################ EXPLORATORY ANALYSIS #########################################

## Outcome/Response Variable
EDCost.pmpm = EDCost/PMPM
dataAdult$PO = PO/PMPM
dataAdult$HO = HO/PMPM

#Histogram of the response variable
par(mfrow=c(2,1))
hist(EDCost.pmpm,breaks=300, xlab="Emergency Department Cost", main="")
hist(log(EDCost.pmpm),breaks=300, xlab="Log-Emergency Department Cost", main="")
log.EDCost.pmpm = log(EDCost.pmpm)

## Response variable vs categorical prediciting variables
par(mfrow=c(2,1))
boxplot(log.EDCost.pmpm ~ State, main = "Variation of log of ED costs by state")
boxplot(log.EDCost.pmpm ~ Urbanicity, main = "Variation of log of ED costs by urbanicity")


## Scatterplot matrix plots
library(car)
par(mfrow=c(1,1))
## Response vs Utilization
scatterplotMatrix(~log(EDCost.pmpm)+HO+PO,smooth=FALSE)
## Response vs Population Characteristics
scatterplotMatrix(~log(EDCost.pmpm)+BlackPop+WhitePop+OtherPop+HealthyPop+ChronicPop+ComplexPop,smooth=FALSE)
## Response vs Social and Economic Environment Characteristics
scatterplotMatrix(~log(EDCost.pmpm)+Unemployment+Income+Poverty+Education+Accessibility+Availability+ProvDensity,smooth=FALSE)
## Response vs County Health Rankings
scatterplotMatrix(~log(EDCost.pmpm)+RankingsPCP+RankingsFood+RankingsHousing+RankingsExercise+RankingsSocial,smooth=FALSE)

library(corrplot)
corr = cor(cbind(log.EDCost.pmpm,dataAdult[,-c(1,2,3,4,5,18)]))
corrplot(corr)

##############################################################################################
################# FULL MODEL: Fitted Model & Residual Analysis ###############################
## Exclude scaling and confounding variable; exclude GEOID;
dataAdult.red = dataAdult[,-c(1,3,4,5,10,13)] 
fullmodel = lm(log(EDCost.pmpm)~ ., data = dataAdult.red)
summary(fullmodel)

### Residual Analysis

full.resid = residuals(fullmodel)
cook = cooks.distance(fullmodel)
par(mfrow=c(2,2))
## Check outliers
influencePlot(fullmodel)
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
## Check Normality
abline(0,0,col="red")
qqPlot(full.resid, ylab="Residuals", main = "")
hist(full.resid, xlab="Residuals", main = "",nclass=30,col="orange")

full.fitted = fitted(fullmodel)
par(mfrow=c(1,1))
plot(full.fitted,full.resid, xlab="fitted values", ylab="Residuals")

## Check Linearity
crPlots(fullmodel,ylab="")

############################################################################
######################### VARIABLE SELECTION ###############################
## 2^23 =  8388608 models -- not feasible to compare all models
### Apply Stepwise Regression
full = lm(log(EDCost.pmpm)~HealthyPop+ChronicPop+State+Urbanicity+HO+PO+BlackPop+WhitePop+
          Unemployment+Income+Poverty+Education+Accessibility+Availability+ProvDensity+
          RankingsPCP+RankingsFood+RankingsExercise+RankingsSocial)  
minimum = lm(log(EDCost.pmpm)~HealthyPop+ChronicPop)

# Forward
forward.model = step(minimum, scope = list(lower=minimum, upper = full), direction = "forward")
summary(forward.model)     
# Backward
backward.model = step(full, scope = list(lower=minimum, upper = full), direction = "backward")
summary(backward.model)
# Both
both.min.model = step(minimum, scope = list(lower=minimum, upper = full), direction = "both")
summary(both.min.model)
both.full.model =step(full, scope = list(lower=minimum, upper = full), direction = "both")
summary(both.full.model)

## Compare full model to selected model
reg.step = lm(log(EDCost.pmpm)~HealthyPop+ChronicPop+State+HO+Education+ProvDensity+
              RankingsPCP+Accessibility+Availability+PO+Urbanicity+BlackPop+WhitePop+RankingsFood) 
anova(reg.step, full)


## Apply Lasso /Elastic Net
library(glmnet)
predictors = as.matrix(dataAdult[,-c(1,2,3,4,5,10,13,18)])
AL = rep(0,length(State))
AL[as.numeric(State)==1] = 1
AR = rep(0,length(State))
AR[as.numeric(State)==2] = 1
LA = rep(0,length(State))
LA[as.numeric(State)==3] = 1
NC = rep(0,length(State))
NC[as.numeric(State)==4] = 1
predictors = cbind(predictors,AR,LA,NC)
rural =  rep(0,length(Urbanicity))
rural[as.numeric(Urbanicity)==1] = 1
suburban =  rep(0,length(Urbanicity))
suburban[as.numeric(Urbanicity)==2] = 1
urban =  rep(0,length(Urbanicity))
urban[as.numeric(Urbanicity)==3] = 1
predictors = cbind(predictors,suburban,urban)
## Lasso Regression
# Find the optimal lambda using 10-fold CV 
lassomodel.cv=cv.glmnet(predictors,log(EDCost.pmpm),alpha=1,nfolds=10)
## Fit lasso model with 100 values for lambda
lassomodel = glmnet(predictors,log(EDCost.pmpm), alpha = 1, nlambda = 100)
## Plot coefficient paths
plot(lassomodel,xvar="lambda",lwd=2,label=TRUE)
abline(v=log(lassomodel.cv$lambda.min),col='black',lty = 2,lwd=2)
## Extract coefficients at optimal lambda
coef(lassomodel,s=enetmodel.cv$lambda.min)


## Elastic Net Regression
# Find the optimal lambda using 10-fold CV  
enetmodel.cv=cv.glmnet(predictors,log(EDCost.pmpm),alpha=0.5,nfolds=10)
## Fit lasso model with 100 values for lambda
enetmodel = glmnet(predictors,log(EDCost.pmpm), alpha = 0.5, nlambda = 100)
## Plot coefficient paths
plot(enetmodel,xvar="lambda",label=T, lwd=2)
abline(v=log(enetmodel.cv$lambda.min),col='black',lty = 2,lwd=2)
## Extract coefficients at optimal lambda
coef(enetmodel,s=enetmodel.cv$lambda.min)


library(lars)
object = lars(x = predictors, y = log(EDCost.pmpm))
plot(object)
object$Cp

plot.lars(object, xvar="df", plottype="Cp")

############# Residual Analysis for Selected Model ########################

red.resid = residuals(reg.step)
red.cook = cooks.distance(reg.step)
par(mfrow=c(2,2))
## Check outliers
influencePlot(reg.step)
plot(red.cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
## Check Normality
abline(0,0,col="red")
qqPlot(red.resid, ylab="Residuals", main = "")
hist(red.resid, xlab="Residuals", main = "",nclass=30,col="orange")

red.fitted = fitted(reg.step)
par(mfrow=c(1,1))
plot(red.fitted,red.resid, xlab="fitted values", ylab="Residuals")

## Check Linearity
crPlots(reg.step,ylab="")

##################### Remove Outlier ######################################
dataAdult.no.out = dataAdult[-909,]
EDCost.pmpm.no.out = EDCost.pmpm[-909]
full = lm(log(EDCost.pmpm.no.out)~HealthyPop+ChronicPop+State+Urbanicity+HO+PO+BlackPop+WhitePop+
          Unemployment+Income+Poverty+Education+Accessibility+Availability+ProvDensity+
          RankingsPCP+RankingsFood+RankingsExercise+RankingsSocial, data=dataAdult.no.out)  
minimum = lm(log(EDCost.pmpm.no.out)~HealthyPop+ChronicPop, data=dataAdult.no.out)
     
# Backward
backward.model = step(full, scope = list(lower=minimum, upper = full), direction = "backward")
summary(backward.model)

reg.step.no.out = lm(log(EDCost.pmpm.no.out)~HealthyPop+ChronicPop+State+HO+Education+ProvDensity+RankingsPCP+
              Accessibility+Availability+PO+Urbanicity+BlackPop+WhitePop+RankingsFood, data = dataAdult.no.out)
summary(reg.step.no.out)

############################################################################
######################### PREDICTION: Interventions ########################

Availability = dataAdult.no.out$Availability
Availability.interv = Availability 
Availability.interv[Availability>=0.5] = 0.5
newdata=dataAdult.no.out
newdata$Availability=Availability.interv
index = which(Availability>=0.5)

preddata =  newdata[,-c(1,3,4,5,10,11,14,15,16,25)]
EDCost.predict = predict(reg.step.no.out, preddata,interval="prediction")[,1]

EDCost.diff.fitted = exp(fitted(reg.step.no.out)) - exp(EDCost.predict)
hist(EDCost.diff.fitted[index],xlab="Difference in Expected versus Predicted ED Cost",main="",col="orange")
summary(EDCost.diff.fitted[index])

EDCost.diff.observed = EDCost.pmpm[-909] - exp(EDCost.predict)
summary(EDCost.diff.observed[index])
hist(EDCost.diff.observed[index],xlab="Difference in Observed versus Predicted ED Cost",main="",col="brown")



## Intervention in increasing utilization of PO
PO = dataAdult.no.out$PO
thresh = median(PO)
PO.interv = PO
PO.interv[(PO)<=thresh] = thresh
newdata=dataAdult.no.out
newdata$PO=PO.interv

EDCost.predict = predict(reg.step.no.out, newdata,interval="prediction")[,1]
EDCost.diff.avail = EDCost.pmpm[-909] - exp(EDCost.predict)
summary(EDCost.diff.avail)
hist(EDCost.diff.avail,xlab="Difference in ED Cost",main="",col="orange")

############################### FINDINGS #####################################

boxplot(ED/PMPM~State,xlab="ED Utilization")


