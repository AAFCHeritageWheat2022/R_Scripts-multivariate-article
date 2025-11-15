setwd("")
getwd()


data20<-read.csv(".csv",header=T)
names(data20)

library(PerformanceAnalytics)
chart.Correlation(data20 [,10:25], histogram = TRUE, method = "pearson")

# sjplot direct correrlation
library(sjPlot)

tab_corr(data20[,10:25],p.numeric = TRUE, triangle = "lower", file="correlation_dtiysiall.doc")

