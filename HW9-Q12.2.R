#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

#seed
set.seed(3)

library(FrF2)

design1<-FrF2(nruns=16,nfactors=10
             ,factor.names = c("large yard","mansion", "2 plus bedrooms","2 plus bathrooms"
                               ,"2 car garage","central heating and cooling","hardwood floors"
                               , "solar roof","great school district","1 storey"))

design.info(design1)
design1

summary(design1)
design1$large.yard

par(mar=c(4,4,4,4))
plot(design1)
desnum(design1)
run.order(design1)
plot(design, cex = 1.2, cex.lab = 1.2, cex.axis = 1.2, main = "Main effects plot for MI", cex.main = 2)
MEPlot(design, abbrev = 5, cex.xax = 1.6, cex.main = 2)

summary(lm(design))
