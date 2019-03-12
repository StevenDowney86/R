library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)


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

#use Minimum covariance determinant estimator and create function
covMcdEstimate <- covMcdEstimator(ASSETSRETURNS_TIMESERIES)
fastCovMcdEstimator <-
  function(x, spec = NULL, ...)
    covMcdEstimate

#Next we define the portfolio specification
covMcdSpec <- portfolioSpec()
setEstimator(covMcdSpec) <- "fastCovMcdEstimator"
setNFrontierPoints(covMcdSpec) <- 25

#optimize the MCD robustified portfolio (with long-only default constraints)
covMcdFrontier <- portfolioFrontier(
  data = ASSETSRETURNS_TIMESERIES, spec = covMcdSpec)
print(covMcdFrontier)

#Create Robust Effecient Frontier
setNFrontierPoints(covMcdSpec) <- 40
covMcdFrontier <- portfolioFrontier(
  data = ASSETSRETURNS_TIMESERIES, spec = covMcdSpec)
tailoredFrontierPlot(
  covMcdFrontier,
  mText = "MCD Robustified MV Portfolio",
  risk = "Sigma")

#To display the weights, risk attributions and covariance risk budgets for
#the MCD robustified portfolio in the left-hand column and the same plots
#for the sample covariance MV portfolio in the right-hand column of a figure

## MCD robustified portfolio
par(mfcol = c(3, 2), mar = c(3.5, 4, 4, 3) + 0.1)
col = qualiPalette(30, "Dark2")
weightsPlot(covMcdFrontier, col = col)
text <- "MCD"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(covMcdFrontier, col = col)
covRiskBudgetsPlot(covMcdFrontier, col = col)
## Sample covariance MV portfolio
longSpec <- portfolioSpec()
setNFrontierPoints(longSpec) <- 20
longFrontier <- portfolioFrontier(data = ASSETSRETURNS_TIMESERIES, spec = longSpec)
col = qualiPalette(30, "Set1")
weightsPlot(longFrontier, col = col)
text <- "COV"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(longFrontier, col = col)
covRiskBudgetsPlot(longFrontier, col = col)


