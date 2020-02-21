rm(list=ls())
setwd("C:\\RProgs")

load('AmesHousing.RData')
head(house,0)
summary(house)
attach(house)
house1 = na.omit(house)
nrow(house)
nrow(house1)
unique(house$Foundation)
library(car)
scatterplotMatrix(~ SalePrice + MS.Zoning + Lot.Area + Lot.Shape + Land.Contour , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Neighborhood + Condition.1 + Bldg.Type + House.Style , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Roof.Style + Mas.Vnr.Type + Mas.Vnr.Area + Exter.Qual , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Exter.Cond + Foundation + Bsmt.Qual + Bsmt.Cond , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + BsmtFin.SF + Total.Bsmt.SF + Heating.QC + Central.Air , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + X1st.Flr.SF + Gr.Liv.Area + Bedroom.AbvGr + Kitchen.Qual , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + TotRms.AbvGrd + Fireplaces + Fireplace.Qu + Garage.Type , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Garage.Finish + Garage.Cars + Garage.Area + Paved.Drive , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Wood.Deck.SF + Open.Porch.SF + Enclosed.Porch + X3Ssn.Porch , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice + Screen.Porch + Pool.Area + Fence + Sale.Type , data = house, smooth=FALSE)
scatterplotMatrix(~ SalePrice +  Sale.Condition + Yr.Sold  + Bath , data = house, smooth=FALSE)


X = model.matrix(lm(SalePrice ~ ., data = house))[,-1]
X = cbind(house$SalePrice, X)
library(corrplot)
corrplot(cor(X), tl.cex = .9)


#X1 = model.matrix(lm(SalePrice ~ MS.Zoning + Lot.Area + Lot.Shape + Land.Contour, data = house))[,-1]
#X1 = cbind(house$SalePrice, X1)
#corrplot(cor(X1), tl.cex = .9)

model1=model1 = lm(SalePrice ~ ., data = house)
summary(model1)


# residual analysis
### Residual Analysis

full.resid = residuals(model1)
cook = cooks.distance(model1)
par(mfrow=c(2,2))
## Check outliers

influencePlot(model1)
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
## Check Normality
abline(0,0,col="red")
qqPlot(full.resid, ylab="Residuals", main = "")
hist(full.resid, xlab="Residuals", main = "",nclass=30,col="orange")

full.fitted = fitted(model1)
par(mfrow=c(1,1))
plot(full.fitted,full.resid, xlab="fitted values", ylab="Residuals")

## Check Linearity
crPlots(model1,ylab="")

plot(model1)

AIC(model1)

drop1(model1)
drop1(model2)

library(MASS)
stepAIC(model1) 


##--lambda --
library(glmnet)
lassomodel.cv=cv.glmnet(data.matrix(house[,-c(1,3, 4,5,6,7,8,12,13,14,18,24,32,44,45,47)]),log(house$SalePrice),alpha=1,nfolds=10)

modelb = boxCox(lm(SalePrice ~ ., data = house),lassomodel.cv$lambda.min)
summary(modelb)


library(forecast)
# to find optimal lambda
lambda = BoxCox.lambda( house$SalePrice )

##--- model2 -- #

model2 = lm(log(SalePrice) ~ ., data = house)
summary(model2)

### Residual Analysis

log.resid = residuals(model2)
log.cook = cooks.distance(model2)
par(mfrow=c(2,2))
## Check outliers
influencePlot(model2)
plot(log.cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
## Check Normality
abline(0,0,col="red")
qqPlot(log.resid, ylab="Residuals", main = "")
hist(log.resid, xlab="Residuals", main = "",nclass=30,col="orange")

log.fitted = fitted(model2)
par(mfrow=c(1,1))
plot(log.fitted,log.resid, xlab="fitted values", ylab="Residuals")

## Check Linearity
crPlots(model2,ylab="")

plot(model2)

drop1(model2)

head(house[,-c(1,3, 4,5,6,7,8,12,13,14,18,24,32,44,45,47)])
head(house)


############################################################################
######################### VARIABLE SELECTION ###############################
## 2^23 =  8388608 models -- not feasible to compare all models
### Apply Stepwise Regression

full = lm(SalePrice ~ .,data=house)  
minimum = lm(SalePrice ~ Overall.Qual + Gr.Liv.Area,data=house)

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

## Compare full model to selected model
#reg.step = lm(log(EDCost.pmpm)~HealthyPop+ChronicPop+State+HO+Education+ProvDensity+
#                RankingsPCP+Accessibility+Availability+PO+Urbanicity+BlackPop+WhitePop+RankingsFood) 
#anova(reg.step, full)


## Apply Lasso /Elastic Net
library(glmnet)
summary(house)
predictors = data.matrix(house[,-c(1,3, 4,5,6,7,8,12,13,14,18,24,32,44,45,47)])
gatt=rep(0,length(Garage.Type))
gatt[Garage.Type=="Attchd"] = 1
gdet=rep(0,length(Garage.Type))
gdet[Garage.Type=="Detchd"] = 1
gno=rep(0,length(Garage.Type))
gno[Garage.Type=="No"] = 1
goth=rep(0,length(Garage.Type))
goth[Garage.Type=="Other"] = 1
predictors = cbind(predictors,gatt,gdet,gno,goth)


slconabnrml =  rep(0,length( Sale.Condition))
slconabnrml[Sale.Condition=="Abnormal"] = 1
slcondoth =  rep(0,length( Sale.Condition))
slcondoth[Sale.Condition=="Other"] = 1
slcondnorm =  rep(0,length( Sale.Condition))
slcondnorm[Sale.Condition=="Normal"] = 1
slcondpart =  rep(0,length( Sale.Condition))
slcondpart[Sale.Condition=="Partial"] = 1
predictors = cbind(predictors,slconabnrml,slcondoth,slcondnorm,slcondpart)

cntlrairY =  rep(0,length(Central.Air))
cntlrairY[Central.Air=="Y"] = 1
cntlrairN =  rep(0,length(Central.Air))
cntlrairN[Central.Air=="N"] = 1
predictors = cbind(predictors,cntlrairY,cntlrairN)

foundBrik =  rep(0,length(Foundation))
foundBrik[Foundation=="BrikTil"] = 1
foundCblk =  rep(0,length(Foundation))
foundCblk[Foundation=="CBlock"] = 1
foundPconc =  rep(0,length(Foundation))
foundPconc[Foundation=="PConc"] = 1
foundOth =  rep(0,length(Foundation))
foundOth[Foundation=="Other"] = 1
predictors = cbind(predictors,foundBrik,foundCblk,foundPconc,foundOth)

## Lasso Regression
# Find the optimal lambda using 10-fold CV 
lassomodel.cv=cv.glmnet(predictors,SalePrice,alpha=1,nfolds=10)
## Fit lasso model with 100 values for lambda
lassomodel = glmnet(predictors,SalePrice, alpha = 1, nlambda = 100)
## Plot coefficient paths
plot(lassomodel,xvar="lambda",lwd=2,label=TRUE)
abline(v=log(lassomodel.cv$lambda.min),col='black',lty = 2,lwd=2)
## Extract coefficients at optimal lambda
coef(lassomodel,s=lassomodel.cv$lambda.min)


## Elastic Net Regression
# Find the optimal lambda using 10-fold CV  
enetmodel.cv=cv.glmnet(predictors,SalePrice,alpha=0.5,nfolds=10)
## Fit lasso model with 100 values for lambda
enetmodel = glmnet(predictors,SalePrice, alpha = 0.5, nlambda = 100)
## Plot coefficient paths
plot(enetmodel,xvar="lambda",label=T, lwd=2)
abline(v=log(enetmodel.cv$lambda.min),col='black',lty = 2,lwd=2)
## Extract coefficients at optimal lambda
coef(enetmodel,s=enetmodel.cv$lambda.min)

AIC(enetmodel)



##gglasso
summary(house)

MS.ZoningRL =  rep(0,length(MS.Zoning))
MS.ZoningRL[MS.Zoning=="RL"] = 1
MS.ZoningRM =  rep(0,length(MS.Zoning))
MS.ZoningRM[MS.Zoning=="RM"] = 1

Lot.ShapeReg =  rep(0,length(Lot.Shape))
Lot.ShapeReg[Lot.Shape=="Reg"] = 1
Lot.ShapeIR1 =  rep(0,length(Lot.Shape))
Lot.ShapeIR1[Lot.Shape=="IR1"] = 1

gpred=cbind(MS.ZoningRL,MS.ZoningRM,Lot.Area,Lot.ShapeReg,Lot.ShapeIR1) 

Land.ContourHLS =  rep(0,length(Land.Contour))
Land.ContourHLS[Land.Contour=="HLS"] = 1
Land.ContourLow =  rep(0,length(Land.Contour))
Land.ContourLow[Land.Contour=="Low"] = 1
Land.ContourLvl =  rep(0,length(Land.Contour))
Land.ContourLvl[Land.Contour=="Lvl"] = 1
gpred=cbind(gpred,Land.ContourHLS,Land.ContourLow,Land.ContourLvl)
#Land.ContourHLS      
#Land.ContourLow      
#Land.ContourLvl      

NeighborhoodBlueste	=  rep(0,length(Neighborhood))
NeighborhoodBrDale	=  rep(0,length(Neighborhood))
NeighborhoodBrkSide	=  rep(0,length(Neighborhood))
NeighborhoodClearCr	=  rep(0,length(Neighborhood))
NeighborhoodCollgCr	=  rep(0,length(Neighborhood))
NeighborhoodCrawfor	=  rep(0,length(Neighborhood))
NeighborhoodEdwards	=  rep(0,length(Neighborhood))
NeighborhoodGilbert	=  rep(0,length(Neighborhood))
NeighborhoodGreens	=  rep(0,length(Neighborhood))
NeighborhoodGrnHill	=  rep(0,length(Neighborhood))
NeighborhoodIDOTRR	=  rep(0,length(Neighborhood))
NeighborhoodMeadowV	=  rep(0,length(Neighborhood))
NeighborhoodMitchel	=  rep(0,length(Neighborhood))
NeighborhoodNAmes	=  rep(0,length(Neighborhood))
NeighborhoodNoRidge	=  rep(0,length(Neighborhood))
NeighborhoodNPkVill	=  rep(0,length(Neighborhood))
NeighborhoodNridgHt	=  rep(0,length(Neighborhood))
NeighborhoodNWAmes	=  rep(0,length(Neighborhood))
NeighborhoodOldTown	=  rep(0,length(Neighborhood))
NeighborhoodSawyer	=  rep(0,length(Neighborhood))
NeighborhoodSawyerW	=  rep(0,length(Neighborhood))
NeighborhoodSomerst	=  rep(0,length(Neighborhood))
NeighborhoodStoneBr	=  rep(0,length(Neighborhood))
NeighborhoodSWISU	=  rep(0,length(Neighborhood))
NeighborhoodTimber	=  rep(0,length(Neighborhood))
NeighborhoodVeenker	=  rep(0,length(Neighborhood))
NeighborhoodBlueste[Neighborhood=="Blueste"]=1
NeighborhoodBrDale[Neighborhood=="BrDale"]=1
NeighborhoodBrkSide[Neighborhood=="BrkSide"]=1
NeighborhoodClearCr[Neighborhood=="ClearCr"]=1
NeighborhoodCollgCr[Neighborhood=="CollgCr"]=1
NeighborhoodCrawfor[Neighborhood=="Crawfor"]=1
NeighborhoodEdwards[Neighborhood=="Edwards"]=1
NeighborhoodGilbert[Neighborhood=="Gilbert"]=1
NeighborhoodGreens[Neighborhood=="Greens"]=1
NeighborhoodGrnHill[Neighborhood=="GrnHill"]=1
NeighborhoodIDOTRR[Neighborhood=="IDOTRR"]=1
NeighborhoodMeadowV[Neighborhood=="MeadowV"]=1
NeighborhoodMitchel[Neighborhood=="Mitchel"]=1
NeighborhoodNAmes[Neighborhood=="NAmes"]=1
NeighborhoodNoRidge[Neighborhood=="NoRidge"]=1
NeighborhoodNPkVill[Neighborhood=="NPkVill"]=1
NeighborhoodNridgHt[Neighborhood=="NridgHt"]=1
NeighborhoodNWAmes[Neighborhood=="NWAmes"]=1
NeighborhoodOldTown[Neighborhood=="OldTown"]=1
NeighborhoodSawyer[Neighborhood=="Sawyer"]=1
NeighborhoodSawyerW[Neighborhood=="SawyerW"]=1
NeighborhoodSomerst[Neighborhood=="Somerst"]=1
NeighborhoodStoneBr[Neighborhood=="StoneBr"]=1
NeighborhoodSWISU[Neighborhood=="SWISU"]=1
NeighborhoodTimber[Neighborhood=="Timber"]=1
NeighborhoodVeenker[Neighborhood=="Veenker"]=1

gpred=cbind(gpred,NeighborhoodBlueste,NeighborhoodBrDale,NeighborhoodBrkSide,NeighborhoodClearCr,NeighborhoodCollgCr,NeighborhoodCrawfor,NeighborhoodEdwards,NeighborhoodGilbert,NeighborhoodGreens,NeighborhoodGrnHill)
gpred=cbind(gpred,NeighborhoodIDOTRR   ,NeighborhoodMeadowV  ,NeighborhoodMitchel  ,NeighborhoodNAmes    ,NeighborhoodNoRidge  ,NeighborhoodNPkVill  ,NeighborhoodNridgHt  ,NeighborhoodNWAmes   ,NeighborhoodOldTown  ,NeighborhoodSawyer   ,NeighborhoodSawyerW  ,NeighborhoodSomerst  ,NeighborhoodStoneBr  ,NeighborhoodSWISU    ,NeighborhoodTimber   ,NeighborhoodVeenker)
            
summary(gpred)
colnames(gpred)

grp=c(1,1,2,3,3,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5)

library(gglasso)
fitgglasso=gglasso(x=gpred,y=SalePrice,group=grp,loss='ls')
coef.mat=fitgglasso$beta
#coef.mat

#g1=max(which(coef.mat[1,]==0))
#g1

#fitgglasso$b0[g1]

fit.cv=cv.gglasso(x=gpred,y=SalePrice,group=grp,nfolds=10)
lmbda=fit.cv$lambda.1se
(coefs=coef.gglasso(object=fitgglasso,s=lmbda))
