rm(list=ls())
set.seed(3)
par(mfrow=c(1,1))
setwd("C:\\RProgs")
##### Final Q1 #####

data = read.csv("atlanta_flights.csv", header=TRUE, sep=",")

# Exploratory Analysis
plot(data$DISTANCE, data$DEPARTURE_DELAY, ylab = "Departure Delay", xlab = "Distance (in miles)")
plot(data$DAY_OF_WEEK, data$DEPARTURE_DELAY, ylab = "Departure Delay", xlab = "Day of the Week")
plot(data$AIRLINE, data$DEPARTURE_DELAY, ylab = "Departure Delay", xlab = "Airline")


# Fit a model
model1 = lm(data$DEPARTURE_DELAY ~ data$DAY_OF_WEEK + data$DISTANCE + data$AIRLINE + data$SCHEDULED_ARRIVAL + data$SCHEDULED_DEPARTURE + data$DESTINATION_AIRPORT)
summary(model1)

# Stepwise model selection
library(MASS)
step = stepAIC(model1,data=data, direction = "both")
summary(step)

s# Selected model
step_model = lm(data$DEPARTURE_DELAY ~ data$AIRLINE + data$DAY_OF_WEEK + data$SCHEDULED_DEPARTURE + data$DESTINATION_AIRPORT)
summary(step_model)

# Cooks distance
cook = cooks.distance(step_model)
which(cook > 1)

newdata = data[-578,]

step_model2 = lm(newdata$DEPARTURE_DELAY ~ newdata$AIRLINE + newdata$DAY_OF_WEEK + newdata$SCHEDULED_DEPARTURE + newdata$DESTINATION_AIRPORT)
summary(step_model2)

par(mfrow=c(2,2))
plot(step_model2)

confint(step_model2, level = .99)

SSE = sum((step_model2$residuals)^2)
SSE

plot(newdata$AIRLINE,newdata$DEPARTURE_DELAY,ylab = "Departure Delay", xlab = "Airline")
plot(newdata$DAY_OF_WEEK,newdata$DEPARTURE_DELAY,ylab = "Departure Delay", xlab = "Day of the Week")
plot(newdata$SCHEDULED_DEPARTURE,newdata$DEPARTURE_DELAY,ylab = "Departure Delay", xlab = "Scheduled Departure")
plot(newdata$DESTINATION_AIRPORT,newdata$DEPARTURE_DELAY,ylab = "Departure Delay", xlab = "Destination Airport")


##### Final Q2 #####

echo = read.csv("echo.csv", header = TRUE, sep = ",")
loggmodel = glm(still_alive ~ age + fractional_shortening + lvdd , family = "binomial", data=echo)
summary(loggmodel)
loggmodel$AIC

# Exploratory Analysis
boxplot(age ~ still_alive,data=echo, ylab = "age",xlab ="Still alive")
boxplot(fractional_shortening ~ still_alive,data=echo,ylab = "Fractional shortening",xlab ="Still alive")
boxplot(lvdd ~ still_alive,data=echo,ylab = "lvdd",xlab ="Still alive")
boxplot(survival ~ still_alive, data=echo, ylab = "Months of survival", xlab = "Still alive")

