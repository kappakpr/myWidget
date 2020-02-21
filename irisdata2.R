#Clear environment
rm(list=ls())

#set working directory
setwd("C:\\RProgs")

irisdata = read.table("iris.txt",header=TRUE)
str(irisdata)
summary(irisdata)

#Rs random number generators for mixing up data
set.seed(3)

head(irisdata)
table(irisdata$Species)

#create a random index
irisdata_mix = runif(nrow(irisdata))
str(irisdata_mix)
head(irisdata,30)

#randomized dataset
irisdata2=irisdata[order(irisdata_mix),]
str(irisdata2)
head(irisdata2,30)
summary(irisdata2)
attributes(irisdata2)

# ggplot2 is an R library for visualizations train.
library(ggplot2)          
ggplot(irisdata2, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point(aes(color = Species)) + scale_x_continuous("Sepal Length", breaks = waiver())+ scale_y_continuous("Sepal Width", breaks = waiver())+ theme_bw()+ stat_ellipse() + labs(title="Sepal Length vs Width Scatterplot")
ggplot(irisdata2, aes(Petal.Length, Petal.Width,color = Species)) + geom_point(aes(color = Species)) + scale_x_continuous("Petal Length", breaks = waiver())+ scale_y_continuous("Petal Width", breaks = waiver())+ theme_bw() + stat_ellipse() + labs(title="Petal Length vs Width Scatterplot")
ggplot(irisdata2, aes(Sepal.Length, Petal.Length, color = Species)) + geom_point(aes(color = Species)) + scale_x_continuous("Sepal Length", breaks = waiver())+ scale_y_continuous("Petal Length", breaks = waiver())+ theme_bw()+ stat_ellipse() + labs(title="Sepal vs Petal Length Scatterplot")
ggplot(irisdata2, aes(Sepal.Width, Petal.Width, color = Species)) + geom_point(aes(color = Species)) + scale_x_continuous("Sepal Width", breaks = waiver())+ scale_y_continuous("Petal Width", breaks = waiver())+ theme_bw()+ stat_ellipse() + labs(title="Sepal vs Petal Width Scatterplot")

library("car")
scatterplotMatrix(irisdata2[1:5])

irisdata3 <-irisdata2[-5]
irisdata3 <- as.data.frame(scale(irisdata3))
sapply(irisdata2[1:4],mean); sapply(irisdata2[1:4],sd)
sapply(irisdata3,mean); sapply(irisdata3[1:4],sd)

kc0 <- kmeans(irisdata2[1:4],3)
kc0$centers
kc0$size
table(irisdata2$Species,kc0$cluster)

kc1 <- kmeans(irisdata2[1:4],7)
kc1$centers
kc1$size
table(irisdata2$Species,kc1$cluster)

kc <- kmeans(irisdata2[1:4],3, nstart = 25, iter.max = 100,
	algorithm = "Hartigan-Wong")
kc$center
table(irisdata2$Species,kc$cluster)

kc2 <- kmeans(irisdata2[1:4],7, nstart = 25, iter.max = 100,
	algorithm = "Hartigan-Wong")
table(irisdata2$Species,kc2$cluster)

library(magrittr)
library(dplyr)
library(broom)
#irisdata3 <-irisdata2[1:4]
kclusts <- data.frame(k=1:9) %>% group_by(k) %>% do(kclust=kmeans(irisdata3, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], irisdata3))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))
p1 <- ggplot(assignments, aes(Sepal.Width, Sepal.Length)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k);p1
ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + scale_x_continuous("k", breaks = seq(0,8,1))+ scale_y_continuous("tot.withinss", breaks = waiver())
