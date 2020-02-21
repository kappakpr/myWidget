rm(list=ls())
setwd("C:\\RProgs")

data = read.csv("WorldCupMatches.csv",header=TRUE)
head(data,10)
summary(data)
str(data)
data = na.omit(data)

#df[,'x']<-factor(df[,'x'])
data$Year<- factor(data$Year)
#Year = as.factor(Year)
class(Year)

attach(data)


lin_model1 = lm(Home_Team_Goals ~ Year + Stage + Away_Team_Goals + Attendance + Home_Team_Name,data= data)
summary(lin_model1)


cook = cooks.distance(lin_model1)
which(cook > 1)

par(mfrow=c(2,2))

#influencePlot(lin_model1)
#plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
#plot(lin_model1)



plot(lin_model1,cook.levels = c(4/622,0.5,1))

plot(lin_model1,cook.levels = c(0.5))

#step both
# Stepwise model selection
library(MASS)
step = stepAIC(lin_model1,data=data, direction = "both")
summary(step)

lin_model2 = lm(Home_Team_Goals ~ Year + Stage + Away_Team_Goals,data= data)
summary(lin_model2)

confint(lin_model2,level=0.99)

pred_data = data.frame(Year = "1986", Stage = "Group C", Away_Team_Goals = 4.)
pred_data

predict(lin_model2,pred_data)


par(mfrow=c(2,2))
plot(lin_model2)


pairs(Home_Team_Goals ~ Year + Stage + Away_Team_Goals,data=data,main="Simple Scatterplot Matrix")
data1=data[,-c(3,4,5,8,9,10,11,12)]
cor(data1[3:4])


pairs(Home_Team_Goals~.,data=data)

lin_model3 = lm(Home_Team_Goals ~ Year + Stage + Away_Team_Goals + Home_Team_Name,data= data)
summary(lin_model3)
