library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)

#create a Mean-CVaR effecient Frontier for daily returns and risk. You may need to convert 
#to monthly Series

#create return series
getSymbols(c("AGG", "SPY", "VT", "GLD", "IEF"))

ASSETS <- merge(AGG[,6],
                SPY[,6],
                VT[,6],
                GLD[,6],
                IEF[,6])

ASSETSRETURNS <- ASSETS %>%  Return.calculate()

#Convert to TimeSeries Class
ASSETSRETURNS_TIMESERIES <- timeSeries(ASSETSRETURNS)
ASSETSRETURNS_TIMESERIES <- na.omit(ASSETSRETURNS_TIMESERIES)
head(ASSETSRETURNS_TIMESERIES)

#Set portfolio Specifications
longSpec <- portfolioSpec()
setType(longSpec) <- "CVaR"

#set the confidence level
setAlpha(longSpec) <- 0.05

#set the rest of the portfolio specification
setNFrontierPoints(longSpec) <- 5
setSolver(longSpec) <- "solveRglpk.CVAR"
longFrontier <- portfolioFrontier(data = ASSETSRETURNS_TIMESERIES, spec = longSpec,
                                    constraints = "LongOnly")
print(longFrontier)

#Plot effecient frontier - note this may take a few minutes to compute
setNFrontierPoints(longSpec) <- 25
longFrontier <- portfolioFrontier(data = ASSETSRETURNS_TIMESERIES, spec = longSpec,
                                    constraints = "LongOnly")
tailoredFrontierPlot(object = longFrontier, mText = "Mean-CVaR Portfolio - Long Only Constraints",
                       risk = "CVaR")

#create bar chart of weights, weighted return, and covariance risk budget
par(mfrow = c(3, 1), mar = c(3.5, 4, 4, 3) + 0.1)
weightsPlot(longFrontier)
text <- "Mean-CVaR Portfolio - Long Only Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(longFrontier)
covRiskBudgetsPlot(longFrontier)
