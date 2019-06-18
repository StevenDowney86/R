library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)


#choose your symbols

Symbols_to_Download <- c("VT",
                       "AGG",
                       "BNDX",
                       "HDG")

#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "keyhere")
getSymbols(Symbols_to_Download, src = "av", 
           output.size = "full", adjusted = TRUE)

#merge the adjusted columns of each symbol, calculate returns, and omit NA
Symbols_to_Download_Returns <- merge(AGG[,6],BNDX[,6],HDG[,6],VT[,6]) %>% 
  Return.calculate() %>% na.omit()


#create weights for each risk level
Portfolio_Weights_Risk_6 <- c(.04,.06,.1,.8)
Portfolio_Weights_Risk_5 <- c(.08,.12,.2,.6)
Portfolio_Weights_Risk_4 <- c(.12,.18,.25,.45)
Portfolio_Weights_Risk_3 <- c(.22,.33,.25,.2)

#choose a start date and the end date for the risk model
Start_Date  <- "2016-09-01/"
End_Date <- "/2019-03-04"

#wealth index for risk model 3-7
Risk3 <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_3,
                          wealth.index = TRUE, rebalance_on = "months")

Risk4 <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_4,
                          wealth.index = TRUE, rebalance_on = "months")

Risk5 <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_5,
                          wealth.index = TRUE, rebalance_on = "months")

Risk6 <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_6,
                          wealth.index = TRUE, rebalance_on = "months")

Risk7 <- Return.portfolio(Symbols_to_Download_Returns$VT.Adjusted[Start_Date], 
                          weights = 1.0,
                          wealth.index = TRUE, rebalance_on = "months")

#merge the risk model returns
Risk_Model_Wealth_Index <- merge(Risk3,Risk4,Risk5,Risk6,Risk7)

#change the name of the columns to be more intuitive
colnames(Risk_Model_Wealth_Index) <- c("Risk3",
                                  "Risk4",
                                  "Risk5",
                                  "Risk6",
                                  "Risk7")
head(Risk_Model_Wealth_Index)
tail(Risk_Model_Wealth_Index, n = 20)
autoplot(Risk_Model_Wealth_Index, facets = NULL)

#returns for risk 3-7
Risk3returns <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_3,
                          wealth.index = FALSE, rebalance_on = "months")

Risk4returns <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_4,
                          wealth.index = FALSE, rebalance_on = "months")

Risk5returns <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_5,
                          wealth.index = FALSE, rebalance_on = "months")

Risk6returns <- Return.portfolio(Symbols_to_Download_Returns[Start_Date], 
                          weights = Portfolio_Weights_Risk_6,
                          wealth.index = FALSE, rebalance_on = "months")

Risk7returns <- Return.portfolio(Symbols_to_Download_Returns$VT.Adjusted[Start_Date], 
                          weights = 1.0,
                          wealth.index = FALSE, rebalance_on = "months")

#merge the risk model returns
Risk_Model_Returns <- merge(Risk3returns,
                            Risk4returns,
                            Risk5returns,
                            Risk6returns,
                            Risk7returns)

#change the name of the columns to be more intuitive
colnames(Risk_Model_Returns) <- c("Risk3 returns",
                                  "Risk4 returns",
                                  "Risk5 returns",
                                  "Risk6 returns",
                                  "Risk7 returns")

table.AnnualizedReturns(Risk_Model_Returns[End_Date])
charts.PerformanceSummary(Risk_Model_Returns[End_Date])

Risk5
chart.CumReturns(Risk5returns)

#Modeling cash flows in and out on a similar risk model
rm(RiskModelUsed)
initial_contribution <- 14466
cashflow <- 3900
RiskModelUsed <- Risk6returns
daily_cashflow <- cashflow/nrow(RiskModelUsed)
RiskModelUsed$daily_cashflow <- daily_cashflow
RiskModelUsed$InitialContribution <- initial_contribution
RiskModelUsed <- RiskModelUsed["/2019-02-28"]

write.csv(RiskModelUsed, file = "riskmodelused.csv")

#


tail(Risk6)

