library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)


#create return series
getSymbols(c("AGG", "SPY", "VT", "GLD", "GSG"))

ASSETS <- merge(AGG[,6],
                SPY[,6],
                VT[,6],
                GLD[,6],
                GSG[,6])

ASSETSRETURNS <- ASSETS %>%  Return.calculate()

#Convert to TimeSeries Class
ASSETSRETURNS_TIMESERIES <- timeSeries(ASSETSRETURNS)
ASSETSRETURNS_TIMESERIES <- na.omit(ASSETSRETURNS_TIMESERIES)
head(ASSETSRETURNS_TIMESERIES)


#compute the effecient fronter
ASSETSRETURNS_TIMESERIES_Spec <- portfolioSpec()
setNFrontierPoints(ASSETSRETURNS_TIMESERIES_Spec) <- 50
longFrontier <- portfolioFrontier(ASSETSRETURNS_TIMESERIES, ASSETSRETURNS_TIMESERIES_Spec)

#print the data
print(longFrontier)

#plot the data
plot(longFrontier)
1
3
7
8
4
0

#Effecient frontier with labels
setNFrontierPoints(ASSETSRETURNS_TIMESERIES_Spec) <- 25
longFrontier <- portfolioFrontier(ASSETSRETURNS_TIMESERIES, ASSETSRETURNS_TIMESERIES_Spec)
tailoredFrontierPlot(object = longFrontier, mText = "MV Portfolio - LongOnly Constraints",
                     risk = "Cov")


#Show the weight plots by size, weighted returns, and covariance risk budget
weightsPlot(longFrontier)
text <- "Mean-Variance Portfolio - Long Only Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(longFrontier)
covRiskBudgetsPlot(longFrontier)


