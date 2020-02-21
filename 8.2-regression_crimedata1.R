#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#par(mar=c(1,1,1,1))

#set plot directory
#jpeg(file = "C:\\RProgs\\mycrimedataplots.jpeg")

#load data
uscrime=read.table("uscrime.txt",header=TRUE, stringsAsFactors = FALSE)
uscrime1=uscrime
#str(uscrime1)

# all Rs on top are 1s, the data is not randomized as required
#Rs random number generators for mixing up data
set.seed(3)

#summary(uscrime1)
#??uscrime1
#library("car")
#scatterplotMatrix(uscrime1[,-16])
#pairs(uscrime1)
#cor(uscrime1$Po1,uscrime1$Po2)
#cor(uscrime1$Wealth,uscrime1$Ineq)
#cor(uscrime1$M,uscrime1$So)
#cor_matrix = round(cor(uscrime1[,-16]),2)
#cor_matrix
#head(uscrime1)
#table(uscrime1$Crime)
#summary(uscrime1$Crime)
#sd(uscrime1$Crime)
#install.packages("outliers")


#grubbs test to drop the outlier datasets
cat ("The minimum value and index of Crime ", min(uscrime1$Crime), which.min(uscrime1$Crime))
cat (" The maximum value and index of Crime  ", max(uscrime1$Crime), which.max(uscrime1$Crime))
print (" ")
  
library(car)
Boxplot(uscrime1$Crime, main = "US Crime data of number of offenses per 100,000 population" 
, ylab = "Number of offences per 100,000 population",col="grey", outcol="red",id.n = Inf)
boxplot.stats(uscrime1$Crime)$out
  
crime=uscrime1$Crime
#plot without scaling
plot(crime ,ylab="Crime data for US"
	,main = "US crime data without scaling",pch=20,cex=1.7
	,col=ifelse(crime==min(crime) | crime==max(crime)
      ,"darkred", "darkgreen"))
  
text(which(crime==min(crime)),min(crime),labels=min(crime),pos=2)
text(which(crime==max(crime)),max(crime),labels=max(crime),pos=2)

#qqnorm(crime)
#qqnorm(scale(crime))

library(outliers)
# one side outlier
#largest
g1 = grubbs.test(uscrime1$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
print(g1)
  
#smallest
g2=grubbs.test(uscrime1$Crime, type = 10, opposite = TRUE, two.sided = FALSE)
print(g2)
print(g2$p.value)

#loop through grubbs test, removing the highest outlier each time
for (i in 1:10)
{
  
  uscrime1 = uscrime1[-which.max(uscrime1$Crime),]
  summary(uscrime1$Crime)
  
  g1 = grubbs.test(uscrime1$Crime, type = 10, opposite = FALSE, two.sided = FALSE);
  print(g1)
    
    if (g1$p.value > 0.1) # & g2$p.value > 0.1) 
    {
      print ("exit the loop")
      break
    }
    
}

crime=uscrime1$Crime
#plot without scaling
plot(crime ,ylab="Crime data for US"
	,main = "US crime data without scaling",pch=20,cex=1.7
	,col=ifelse(crime==min(crime) | crime==max(crime)
      ,"darkred", "darkgreen"))
  
text(which(crime==min(crime)),min(crime),labels=min(crime),pos=2)
text(which(crime==max(crime)),max(crime),labels=max(crime),pos=2)

#qqnorm(crime )
#summary(uscrime1$Crime)
#sd(uscrime1$Crime)

predictors = colnames(uscrime1)
predictors = predictors[-16]
par(mfrow = c(3,5))
for (i in predictors) {
  eval(parse(text=paste('plot(uscrime1$Crime~uscrime1$',i,',main="Scatterplot of ',i,'",
                        ylab="Crime Rate",pch = 16,cex = 1, col=ifelse(uscrime1$Crime==min(uscrime1$Crime)
                                                                       | uscrime1$Crime==max(uscrime1$Crime)
                                                                       ,"red","blue"))')))
}

#linear regression models
Crime0 <- uscrime$Crime
model0 <- lm(Crime0 ~ .,data = uscrime[,-16])
summary(model0)
#print(model0$residuals)
par(mfrow = c(2, 2))
plot(model0)
#par(mfrow = c(1,1))
#plot(model0$residuals)
#qqnorm(model0$residuals)
#res0 <- shapiro.test(model0$residuals)
#print (res0$p.value )

# par(mfrow = c(1, 1))
# uscrime0 <- uscrime
# Crime0 <- uscrime0$Crime
# uscrime0$predicted <- predict(model0) # Save the predicted values
# uscrime0$residuals <- residuals(model0) # Save the residual values
# library(dplyr)
# #uscrime2 %>% select(Crime, predicted, residuals) %>% head()
# library(ggplot2)
# ggplot(uscrime0, aes(x = Ed, y = Crime)) +
#   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#   geom_segment(aes(xend = Ed, yend = predicted), alpha = .2) +
#   # > Color adjustments made here...
#   geom_point(aes(color = residuals)) +  # Color mapped here
#   scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
#   guides(color = FALSE) +
#   geom_point(aes(y = predicted), shape = 1) +
#   theme_bw()

#dropping data fields identified earlier for correlation, or not so significant.
#uscrime2 = within(uscrime1, rm(So,Po2,LF,M.F,Pop,NW,U1,Wealth,Time))
uscrime2 = uscrime1
#head(uscrime1)
#head(uscrime2)


Crime <- uscrime2$Crime
model1 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,data = uscrime2)
summary(model1)
#print(model1$residuals)
par(mfrow = c(2, 2))
plot(model1)
#plot(model1$residuals)
#plot(model1$fitted.values)
#qqnorm(model1$residuals)
#res <- shapiro.test(model1$residuals)
#print (res$p.value )

# 
# par(mfrow = c(1, 1))
# uscrime2$predicted <- predict(model1) # Save the predicted values
# uscrime2$residuals <- residuals(model1) # Save the residual values
# library(dplyr)
# #uscrime2 %>% select(Crime, predicted, residuals) %>% head()
# library(ggplot2)
# ggplot(uscrime2, aes(x = Ed, y = Crime)) +
#   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#   geom_segment(aes(xend = Ed, yend = predicted), alpha = .2) +
#   # > Color adjustments made here...
#   geom_point(aes(color = residuals)) +  # Color mapped here
#   scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
#   guides(color = FALSE) +
#   geom_point(aes(y = predicted), shape = 1) +
#   theme_bw()

#install.packages("DAAG")
library(DAAG)
par(mfrow = c(1,1))
#5 fold validation
model0_cv <- cv.lm(uscrime,model0,m=5)

model1_cv <- cv.lm(uscrime2,model1,m=5)

predict(model1)

#AIC
AIC(model0)
AIC(model1)

SStot0 <- sum((uscrime$Crime - mean(uscrime$Crime))^2)
SStot1 <- sum((uscrime2$Crime - mean(uscrime2$Crime))^2)

SSres_model0 <- sum(model0$residuals^2)
SSres_model1 <- sum(model1$residuals^2)

SSres_model0_cv <- attr(model0_cv,"ms")*nrow(uscrime)

SSres_model1_cv <- attr(model1_cv,"ms")*nrow(uscrime2)

r2_model0 <- 1 - SSres_model0/SStot0 
#0.803

r2_model1 <- 1 - SSres_model1/SStot1
#0.656

r2_model0_cv <- 1 - SSres_model0_cv/SStot0 
#0.413

r2_model1_cv <- 1 - SSres_model1_cv/SStot1 
#0.489

#Adjusted R square
adjr2_model0 <- 1 - ((1 - r2_model0)*(47 - 1) / (47-15-1))
# 0.708

adjr2_model1 <- 1 - ((1 - r2_model1)*(45 - 1) / (45-6-1))
# 0.601

adjr2_model0_cv <- 1 - ((1 - r2_model0_cv)*(47 - 1) / (47-15-1))
#0.13

adjr2_model1_cv <- 1 - ((1 - r2_model1_cv)*(45 - 1) / (45-6-1))
#0.413

#test data
testdat <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                   NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)

pred_model0 <- predict(model0,testdat)
#155.4349

pred_model1 <- predict(model1,testdat)
#1263.597

#pred_model1_cv <- predict(model1_cv,testdat)

#GLM
Crime <- uscrime$Crime
model0_glm <- glm(Crime ~. , data=uscrime, family = "gaussian")
plot(model0_glm)
summary(model0_glm)

Crime <- uscrime2$Crime
model1_glm <- glm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,data = uscrime2)
summary(model1_glm)
plot(model1_glm)

library(boot)
model0_glm_cv <- cv.glm(uscrime,model0_glm,K=5)
summary(model0_glm_cv)

model1_glm_cv <- cv.glm(uscrime2,model1_glm,K=5)
summary(model1_glm_cv)

pred_model0_glm <- predict(model0_glm,testdat)
#155.4349

pred_model1_glm <- predict(model1_glm,testdat)
#1263.597

pred_model0_glm_Rsq = 1 - model0_glm_cv$delta[1]*nrow(uscrime)/SStot0

pred_model1_glm_Rsq = 1 - model1_glm_cv$delta[1]*nrow(uscrime2)/SStot



#trendfactor 
uscrime3 <- uscrime2
uscrime3$U2Time <- uscrime1$U2 * uscrime1$Time
head(uscrime3)
Crime3 <- uscrime3$Crime
model3 <- lm(Crime3 ~ .,data = uscrime3[,-7])
summary(model3)

#eval(parse(text="5+5"))
#if(!is.null(dev.list())) dev.off()

