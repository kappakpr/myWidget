alpha<-0.05
ttest<-qt(1-alpha/47,46) #use ttest<-qt(alpha/47,46) for testing minimum value as outlier
tcrit<-(46/sqrt(47))*sqrt((ttest^2)/(47-1+ttest^2))
tcrit
library(outliers)
pgrubbs(2.8129,47)
