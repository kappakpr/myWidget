rm(list=ls())
set.seed(3)
par(mfrow=c(1,1))
setwd("C:\\RProgs")
#################Multiple Choices

########Load the data #############
house <- read.csv("tomslee_airbnb_asheville_1498_2017-07-20.csv",header = TRUE)
head(house)
str(house)
summary(house)

##########Build the model##############
model1 <- lm(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms, data = house)

##B1
confint(model1,level = 0.99)

##B2,B3
summary(model1)

##B4
cooks <- cooks.distance(model1)
which(cooks > 1)

##B5
house_new = house[-c(94,95),]
model2 <- lm(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms, data = house_new)
confint(model2)

##B6
new_data = data.frame(bedrooms = 1, accommodates = 2, reviews = 92, overall_satisfaction = 3.5, room_type = 'Private room')
predict(model2,new_data)

#######################Part 3################
##Q1
summary(model2)

##Q2
par(mfrow=c(2,2))
plot(model2)

