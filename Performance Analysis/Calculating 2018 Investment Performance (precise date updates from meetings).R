library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)

#High level this script is trying to teeze out what of our model 6 risk came from 
#Mincorr momentum, Technical Indicators, Strategic Asset Allocation, and Tactical Shifts.
#I go through and enter the monthly allocation for each component of the portfolio and then combine vs.
#risk 6 model benchmark to see where value was added or subtracted and see where we can improve

#The Risk 6 Model Risk Blend is composed of 80% global stocks/10% global bonds/10% global alternative assets, 
#as represented by the FTSE Global All Cap Equity Index (proxied by the Vanguard Total World Stock ETF, ticker VT), 
#the Barclays Global Aggregate Bond Index (proxied by Vanguard Total International Bond ETF, ticker BNDX and iShares 
#Core US Aggregate Bond, Ticker AGG) and the Hedge Fund Research Inc. Fund Weighted Composite Index, as proxied by 
#Proshares Hedge Replication ETF, ticker HDG											

#the main difference with this script is having the dates of our monthly mincorr meeting reflected in the performance data

#this script will download the MinCorr and my preferred Alternative ETFs and Mutual Funds
#from alphavantage and calculate their returns YTD, and will show them in different charts.

start_t<-Sys.time()

#from http://joeystanley.com/blog/custom-themes-in-ggplot2
theme_joey <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

MinCorr_Symbols <- c("EPP",
                     "IEV",
                     "IWB",
                     "EWJ",
                     "VWO",
                     "ILF",
                     "RWO",
                     "GLD",
                     "GSG",
                     "IEF",
                     "SHY",
                     "BWX",
                     "TIP",
                     "LQD",
                     "EMB",
                     "HYG",
                     "TLT",
                     "FXI",
                     "BKLN",
                     "FLOT")

Alternative_ETFs_MFs <- c("HDGE",
                          "TAIL",
                          "GMOM",
                          "QAI",
                          "DIVY",
                          "AMFAX",
                          "MFTNX")

#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols(MinCorr_Symbols, src = "av", 
           output.size = "full", adjusted = TRUE)
getSymbols(Alternative_ETFs_MFs, src = "av", 
           output.size = "full", adjusted = TRUE)


#merge the data
MinCorr_ETFs <- merge(IWB[,6], IEV[,6], EWJ[,6], EPP[,6], VWO[,6], ILF[,6], FXI[,6],
                      RWO[,6], GSG[,6], GLD[,6], SHY[,6], IEF[,6], TLT[,6], BWX[,6], 
                      LQD[,6], FLOT[,6], BKLN[,6], HYG[,6], EMB[,6], TIP[,6], HDGE[,6],
                      TAIL[,6], QAI[,6], DIVY[,6])

MinCorr_ETFs <- na.omit(MinCorr_ETFs)
#Choose the dates you want to use to for analysis
Dates <- "2017-12-29/2018"

#Trim the data to analyze just the dates you want
MinCorr_ETFs <- MinCorr_ETFs[Dates]


#name the column data to be intuitive
colnames(MinCorr_ETFs) <- c("USA Large Cap Equity (IWB)",
                            "Developed Europe Equity (IEV)",
                            "Japan Equity (EWJ)",
                            "Pacific ex-Japan Equity (EPP)",
                            "Emerging Market Equity (VWO)",
                            "Latin America Equity (ILF)",
                            "China Equity (FXI)",
                            "Global Real Estate (RWO)",
                            "Commodities (GSG)",
                            "Gold (GLD)",
                            "USA 1-3 Year Treasuries (SHY)",
                            "USA 7-10 Year Treasuries (IEF)",
                            "USA 20+ Years Treasuries (TLT)",
                            "Int'l Treasury Bonds (BWX)",
                            "USA Inv Grade Bonds (LQD)",
                            "Investment Grade Floating Rate (FLOT)",
                            "High Yield Floating Sen. Loans (BKLN)",
                            "USA High Yield (HYG)",
                            "Emerging Market Bonds (EMB)",
                            "USA TIPS (TIP)",
                            "Ranger Dedicated Short (HDGE)",
                            "Cambria Tail Risk (TAIL)",
                            "IQ Hedge Multi-Strategy (QAI)",
                            "Realtyshares DIVY (DIVY)")
#calculate returns and check
MinCorr_ETFs_Returns <- MinCorr_ETFs %>% Return.calculate() %>% na.omit()

#Choose the dates that were used for actual meeting times that can be used
#for all the rebalancing dates. Probably best to use the date after the meeting
#to reflect what we could actually execute

January_Rebalance_Dates <- "2018-01/2018-02-12"
February_Rebalance_Dates <- "2018-02-13/2018-03-05"
March_Rebalance_Dates <- "2018-03-06/2018-04-10"
April_Rebalance_Dates <- "2018-04-11/2018-05-09"
May_Rebalance_Dates <- "2018-05-10/2018-06-10"
June_Rebalance_Dates <- "2018-06-11/2018-07-17"
July_Rebalance_Dates <- "2018-07-18/2018-08-14"
August_Rebalance_Dates <- "2018-08-15/2018-09-10"
September_Rebalance_Dates <- "2018-09-11/2018-10-08"
October_Rebalance_Dates <- "2018-10-09/2018-11-11"
November_Rebalance_Dates <- "2018-11-12/2018-12-10"
December_Rebalance_Dates <- "2018-12-11/2018"

#enter each month weights for respective Asset Classes/ETF allocations to calculate 
#composite performance 
January_weights <- c(#IWB
                    .15,
                    #IEV
                     .15,
                    #EWJ
                     .13,
                    #EPP
                     .03,
                    #VWO
                     .12,
                    #ILF
                     .03,
                    #FXI
                     .08,
                    #RWO
                     .00,
                    #GSG
                     .06,
                    #GLD
                     .03,
                    #SHY
                     .02,
                    #IEF
                     .00,
                    #TLT
                     .00,
                    #BWX
                     .01,
                    #LQD
                     .01,
                    #FLOT
                     .04,
                    #BKLN
                     .00,
                    #HYG
                     .01,
                    #EMB
                     .01,
                    #TIP
                     .00,
                    #HDGE
                     .03,
                    #TAIL
                     .03,
                    #QAI
                     .03,
                    #DIVY
                     .03)


sum(January_weights) #confirm the weights add up to 1 so you can use return.portfolio function
length(January_weights) #confirm the weights are 24 and the same length as number of columns in MinCorrReturns
#otherwise won't work with return.portfolio function

#create portfolio return
January_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = January_weights, rebalance_on = "months")

#trim the data to be just for the month analyzed
January_Performance <- January_Performance[January_Rebalance_Dates]

February_weights <- c(#IWB
  .18,
  #IEV
  .12,
  #EWJ
  .10,
  #EPP
  .02,
  #VWO
  .13,
  #ILF
  .08,
  #FXI
  .10,
  #RWO
  .00,
  #GSG
  .06,
  #GLD
  .03,
  #SHY
  .02,
  #IEF
  .00,
  #TLT
  .00,
  #BWX
  .01,
  #LQD
  .00,
  #FLOT
  .02,
  #BKLN
  .00,
  #HYG
  .01,
  #EMB
  .00,
  #TIP
  .00,
  #HDGE
  .03,
  #TAIL
  .03,
  #QAI
  .03,
  #DIVY
  .03)

sum(February_weights)
length(February_weights)
February_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = February_weights, rebalance_on = "months")
February_Performance <- February_Performance[February_Rebalance_Dates]

March_weights <- c(#IWB
  .12,
  #IEV
  .12,
  #EWJ
  .14,
  #EPP
  .02,
  #VWO
  .13,
  #ILF
  .08,
  #FXI
  .10,
  #RWO
  .00,
  #GSG
  .08,
  #GLD
  .05,
  #SHY
  .02,
  #IEF
  .00,
  #TLT
  .00,
  #BWX
  .01,
  #LQD
  .00,
  #FLOT
  .02,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .00,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .03)

sum(March_weights)
length(March_weights)
March_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = March_weights, rebalance_on = "months")
March_Performance <- March_Performance[March_Rebalance_Dates]


April_weights <- c(#IWB
  .07,
  #IEV
  .13,
  #EWJ
  .12,
  #EPP
  .02,
  #VWO
  .12,
  #ILF
  .07,
  #FXI
  .09,
  #RWO
  .01,
  #GSG
  .10,
  #GLD
  .08,
  #SHY
  .00,
  #IEF
  .01,
  #TLT
  .00,
  #BWX
  .06,
  #LQD
  .00,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .02,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(April_weights)
length(April_weights)
April_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = April_weights, rebalance_on = "months")
April_Performance <- April_Performance[April_Rebalance_Dates]

May_weights <- c(#IWB
  .11,
  #IEV
  .11,
  #EWJ
  .11,
  #EPP
  .03,
  #VWO
  .06,
  #ILF
  .02,
  #FXI
  .06,
  #RWO
  .02,
  #GSG
  .11,
  #GLD
  .05,
  #SHY
  .10,
  #IEF
  .01,
  #TLT
  .00,
  #BWX
  .05,
  #LQD
  .00,
  #FLOT
  .02,
  #BKLN
  .02,
  #HYG
  .02,
  #EMB
  .00,
  #TIP
  .02,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(May_weights)
length(May_weights)
May_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = May_weights, rebalance_on = "months")
May_Performance <- May_Performance[May_Rebalance_Dates]

June_weights <- c(#IWB
  .11,
  #IEV
  .11,
  #EWJ
  .09,
  #EPP
  .09,
  #VWO
  .06,
  #ILF
  .00,
  #FXI
  .08,
  #RWO
  .04,
  #GSG
  .10,
  #GLD
  .08,
  #SHY
  .08,
  #IEF
  .00,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .00,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .01,
  #EMB
  .02,
  #TIP
  .00,
  #HDGE
  .03,
  #TAIL
  .03,
  #QAI
  .03,
  #DIVY
  .03)

sum(June_weights)
length(June_weights)
June_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = June_weights, rebalance_on = "months")
June_Performance <- June_Performance[June_Rebalance_Dates]

July_weights <- c(#IWB
  .20,
  #IEV
  .09,
  #EWJ
  .09,
  #EPP
  .09,
  #VWO
  .05,
  #ILF
  .00,
  #FXI
  .03,
  #RWO
  .08,
  #GSG
  .08,
  #GLD
  .03,
  #SHY
  .07,
  #IEF
  .02,
  #TLT
  .02,
  #BWX
  .00,
  #LQD
  .02,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .00,
  #TIP
  .00,
  #HDGE
  .03,
  #TAIL
  .03,
  #QAI
  .04,
  #DIVY
  .03)

sum(July_weights)
length(July_weights)
July_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = July_weights, rebalance_on = "months")
July_Performance <- July_Performance[July_Rebalance_Dates]

August_weights <- c(#IWB
  .19,
  #IEV
  .12,
  #EWJ
  .06,
  #EPP
  .06,
  #VWO
  .07,
  #ILF
  .00,
  #FXI
  .03,
  #RWO
  .07,
  #GSG
  .07,
  #GLD
  .03,
  #SHY
  .01,
  #IEF
  .02,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .05,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .09,
  #EMB
  .02,
  #TIP
  .05,
  #HDGE
  .01,
  #TAIL
  .01,
  #QAI
  .02,
  #DIVY
  .02)

sum(August_weights)
length(August_weights)
August_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = August_weights, rebalance_on = "months")
August_Performance <- August_Performance[August_Rebalance_Dates]

September_weights <- c(#IWB
  .16,
  #IEV
  .10,
  #EWJ
  .05,
  #EPP
  .04,
  #VWO
  .06,
  #ILF
  .00,
  #FXI
  .03,
  #RWO
  .05,
  #GSG
  .06,
  #GLD
  .05,
  #SHY
  .01,
  #IEF
  .05,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .03,
  #FLOT
  .01,
  #BKLN
  .00,
  #HYG
  .20,
  #EMB
  .02,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(September_weights)
length(September_weights)
September_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = September_weights, rebalance_on = "months")
September_Performance <- September_Performance[September_Rebalance_Dates]

October_weights <- c(#IWB
  .12,
  #IEV
  .09,
  #EWJ
  .10,
  #EPP
  .03,
  #VWO
  .05,
  #ILF
  .01,
  #FXI
  .03,
  #RWO
  .06,
  #GSG
  .08,
  #GLD
  .03,
  #SHY
  .06,
  #IEF
  .00,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .00,
  #FLOT
  .03,
  #BKLN
  .00,
  #HYG
  .20,
  #EMB
  .03,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(October_weights)
length(October_weights)
October_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = October_weights, rebalance_on = "months")
October_Performance <- October_Performance[October_Rebalance_Dates]

November_weights <- c(#IWB
  .05,
  #IEV
  .09,
  #EWJ
  .04,
  #EPP
  .01,
  #VWO
  .05,
  #ILF
  .02,
  #FXI
  .03,
  #RWO
  .03,
  #GSG
  .01,
  #GLD
  .04,
  #SHY
  .38,
  #IEF
  .00,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .00,
  #FLOT
  .03,
  #BKLN
  .00,
  #HYG
  .11,
  #EMB
  .03,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(November_weights)
length(November_weights)
November_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = November_weights, rebalance_on = "months")
November_Performance <-November_Performance[November_Rebalance_Dates]

December_weights <- c(#IWB
  .09,
  #IEV
  .06,
  #EWJ
  .04,
  #EPP
  .02,
  #VWO
  .07,
  #ILF
  .00,
  #FXI
  .03,
  #RWO
  .04,
  #GSG
  .00,
  #GLD
  .05,
  #SHY
  .38,
  #IEF
  .06,
  #TLT
  .02,
  #BWX
  .00,
  #LQD
  .00,
  #FLOT
  .03,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .03,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(December_weights)
length(December_weights)
December_Performance <- Return.portfolio(MinCorr_ETFs_Returns, weights = December_weights, rebalance_on = "months")
December_Performance <-December_Performance[December_Rebalance_Dates]

#Rbind all of the performance months
Total_Performance1 <- rbind.zoo(January_Performance,
                               February_Performance,
                               March_Performance,
                               April_Performance,
                               May_Performance,
                               June_Performance,
                               July_Performance,
                               August_Performance,
                               September_Performance,
                               October_Performance,
                               November_Performance,
                               December_Performance)

Total_Performance1

chart.CumReturns(Total_Performance1)

##########################################################################################################

#enter each month weights for respective Asset Classes/ETF allocations to calculate Strategic
#VALUE part of the portfolio performance 
January_weights_value <- c(#IWB
  .14,
  #IEV
  .19,
  #EWJ
  .06,
  #EPP
  .04,
  #VWO
  .12,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .02,
  #GLD
  .04,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .02,
  #BWX
  .02,
  #LQD
  .04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .01,
  #TIP
  .04,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .03)


sum(January_weights_value) #confirm the weights add up to 1
length(January_weights_value) #confirm the weights are 24 and the same length as number of columns in MinCorrReturns

#create portfolio return
January_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = January_weights_value, rebalance_on = "months")

#trim the data to be just for the month analyzed
January_Performance_value <- January_Performance_value[January_Rebalance_Dates]

February_weights_value <- c(#IWB
  .14,
  #IEV
  .19,
  #EWJ
  .06,
  #EPP
  .04,
  #VWO
  .12,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .02,
  #GLD
  .04,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .02,
  #BWX
  .02,
  #LQD
  .04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .01,
  #TIP
  .04,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .03)

sum(February_weights_value)
length(February_weights_value)
February_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = February_weights_value, rebalance_on = "months")
February_Performance_value <- February_Performance_value[February_Rebalance_Dates]


March_weights_value <- c(#IWB
  .14,
  #IEV
  .19,
  #EWJ
  .06,
  #EPP
  .04,
  #VWO
  .12,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .02,
  #GLD
  .04,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .02,
  #BWX
  .02,
  #LQD
  .04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .01,
  #TIP
  .04,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .03)

sum(March_weights_value)
length(March_weights_value)
March_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = March_weights_value, rebalance_on = "months")
March_Performance_value <- March_Performance_value[March_Rebalance_Dates]

April_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(April_weights_value)
length(April_weights_value)
April_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = April_weights_value, rebalance_on = "months")
April_Performance_value <- April_Performance_value[April_Rebalance_Dates]

May_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(May_weights_value)
length(May_weights_value)
May_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = May_weights_value, rebalance_on = "months")
May_Performance_value <- May_Performance_value[May_Rebalance_Dates]

June_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(June_weights_value)
length(June_weights_value)
June_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = June_weights_value, rebalance_on = "months")
June_Performance_value <- June_Performance_value[June_Rebalance_Dates]

July_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(July_weights_value)
length(July_weights_value)
July_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = July_weights_value, rebalance_on = "months")
July_Performance_value <- July_Performance_value[July_Rebalance_Dates]

August_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(August_weights_value)
length(August_weights_value)
August_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = August_weights_value, rebalance_on = "months")
August_Performance_value <- August_Performance_value[August_Rebalance_Dates]

September_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(September_weights_value)
length(September_weights_value)
September_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = September_weights_value, rebalance_on = "months")
September_Performance_value <- September_Performance_value[September_Rebalance_Dates]

October_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(October_weights_value)
length(October_weights_value)
October_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = October_weights_value, rebalance_on = "months")
October_Performance_value <- October_Performance_value[October_Rebalance_Dates]

November_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(November_weights_value)
length(November_weights_value)
November_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = November_weights_value, rebalance_on = "months")
November_Performance_value <-November_Performance_value[November_Rebalance_Dates]

December_weights_value <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(December_weights_value)
length(December_weights_value)
December_Performance_value <- Return.portfolio(MinCorr_ETFs_Returns, weights = December_weights_value, rebalance_on = "months")
December_Performance_value <-December_Performance_value[December_Rebalance_Dates]

#Rbind all of the performance months
Total_Performance_value <- rbind.zoo(January_Performance_value,
                                February_Performance_value,
                                March_Performance_value,
                                April_Performance_value,
                                May_Performance_value,
                                June_Performance_value,
                                July_Performance_value,
                                August_Performance_value,
                                September_Performance_value,
                                October_Performance_value,
                                November_Performance_value,
                                December_Performance_value)

Total_Performance_value

chart.CumReturns(Total_Performance_value)

##########################################################################################################

#enter each month weights for respective Asset Classes/ETF allocations to calculate Total Value
#part of the portfolio performance and from there subtract the Strategic Value to extract
#the tactical benefit/cost experienced
January_weights_value_total <- c(#IWB
  .17,
  #IEV
  .17,
  #EWJ
  .05,
  #EPP
  .04,
  #VWO
  .12,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .02,
  #GLD
  .04,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .04,
  #FLOT
  .09,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .01,
  #TIP
  .00,
  #HDGE
  .02,
  #TAIL
  .03,
  #QAI
  .03,
  #DIVY
  .03)


sum(January_weights_value_total) #confirm the weights add up to 1
length(January_weights_value_total) #confirm the weights are 24 and the same length as number of columns in MinCorrReturns

#create portfolio return
January_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = January_weights_value_total, rebalance_on = "months")

#trim the data to be just for the month analyzed
January_Performance_value_total <- January_Performance_value_total[January_Rebalance_Dates]

February_weights_value_total <- c(#IWB
  .12,
  #IEV
  .19,
  #EWJ
  .06,
  #EPP
  .04,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .02,
  #GLD
  .04,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .04,
  #FLOT
  .05,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .01,
  #TIP
  .04,
  #HDGE
  .03,
  #TAIL
  .03,
  #QAI
  .03,
  #DIVY
  .03)

sum(February_weights_value_total)
length(February_weights_value_total)
February_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = February_weights_value_total, rebalance_on = "months")
February_Performance_value_total <- February_Performance_value_total[February_Rebalance_Dates]


March_weights_value_total <- c(#IWB
  .14,
  #IEV
  .19,
  #EWJ
  .06,
  #EPP
  .04,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .00,
  #GLD
  .09,
  #SHY
  .03,
  #IEF
  .02,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .04,
  #FLOT
  .03,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .01,
  #TIP
  .03,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .03)

sum(March_weights_value_total)
length(March_weights_value_total)
March_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = March_weights_value_total, rebalance_on = "months")
March_Performance_value_total <- March_Performance_value_total[March_Rebalance_Dates]

April_weights_value_total <- c(#IWB
  .15,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .10,
  #ILF
  -.04,
  #FXI
  .05,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .08,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(April_weights_value_total)
length(April_weights_value_total)
April_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = April_weights_value_total, rebalance_on = "months")
April_Performance_value_total <- April_Performance_value_total[April_Rebalance_Dates]

May_weights_value_total <- c(#IWB
  .16,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .10,
  #ILF
  -.04,
  #FXI
  .05,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .07,
  #BKLN
  .00,
  #HYG
  .05,
  #EMB
  .00,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(May_weights_value_total)
length(May_weights_value_total)
May_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = May_weights_value_total, rebalance_on = "months")
May_Performance_value_total <- May_Performance_value_total[May_Rebalance_Dates]

June_weights_value_total <- c(#IWB
  .04,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .16,
  #IEF
  .00,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .05,
  #TIP
  -.11,
  #HDGE
  .04,
  #TAIL
  .04,
  #QAI
  .06,
  #DIVY
  .06)

sum(June_weights_value_total)
length(June_weights_value_total)
June_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = June_weights_value_total, rebalance_on = "months")
June_Performance_value_total <- June_Performance_value_total[June_Rebalance_Dates]


July_weights_value_total <- c(#IWB
  .16,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .00,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .01,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .03,
  #DIVY
  .02)

sum(July_weights_value_total)
length(July_weights_value_total)
July_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = July_weights_value_total, rebalance_on = "months")
July_Performance_value_total <- July_Performance_value_total[July_Rebalance_Dates]

August_weights_value_total <- c(#IWB
  .16,
  #IEV
  .20,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .15,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .03,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .01,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(August_weights_value_total)
length(August_weights_value_total)
August_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = August_weights_value_total, rebalance_on = "months")
August_Performance_value_total <- August_Performance_value_total[August_Rebalance_Dates]

September_weights_value_total <- c(#IWB
  .16,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  -.11,
  #GSG
  .01,
  #GLD
  .10,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .02,
  #BKLN
  .00,
  #HYG
  .10,
  #EMB
  .05,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(September_weights_value_total)
length(September_weights_value_total)
September_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = September_weights_value_total, rebalance_on = "months")
September_Performance_value_total <- September_Performance_value_total[September_Rebalance_Dates]


October_weights_value_total <- c(#IWB
  .08,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .07,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .04,
  #BKLN
  .03,
  #HYG
  .00,
  #EMB
  .08,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(October_weights_value_total)
length(October_weights_value_total)
October_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = October_weights_value_total, rebalance_on = "months")
October_Performance_value_total <- October_Performance_value_total[October_Rebalance_Dates]

November_weights_value_total <- c(#IWB
  .08,
  #IEV
  .21,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .07,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .07,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .08,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(November_weights_value_total)
length(November_weights_value_total)
November_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = November_weights_value_total, rebalance_on = "months")
November_Performance_value_total <-November_Performance_value_total[November_Rebalance_Dates]

December_weights_value_total <- c(#IWB
  .16,
  #IEV
  .13,
  #EWJ
  .06,
  #EPP
  .05,
  #VWO
  .13,
  #ILF
  .00,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .01,
  #GLD
  .07,
  #SHY
  .01,
  #IEF
  .03,
  #TLT
  .01,
  #BWX
  .00,
  #LQD
  .01,
  #FLOT
  .07,
  #BKLN
  .00,
  #HYG
  .00,
  #EMB
  .08,
  #TIP
  .01,
  #HDGE
  .02,
  #TAIL
  .02,
  #QAI
  .02,
  #DIVY
  .02)

sum(December_weights_value_total)
length(December_weights_value_total)
December_Performance_value_total <- Return.portfolio(MinCorr_ETFs_Returns, weights = December_weights_value_total, rebalance_on = "months")
December_Performance_value_total <-December_Performance_value_total[December_Rebalance_Dates]

#Rbind all of the performance months
Total_Performance_value_total <- rbind.zoo(January_Performance_value_total,
                                     February_Performance_value_total,
                                     March_Performance_value_total,
                                     April_Performance_value_total,
                                     May_Performance_value_total,
                                     June_Performance_value_total,
                                     July_Performance_value_total,
                                     August_Performance_value_total,
                                     September_Performance_value_total,
                                     October_Performance_value_total,
                                     November_Performance_value_total,
                                     December_Performance_value_total)

Total_Performance_value_total

chart.CumReturns(Total_Performance_value_total)

#####################################################################################################

#Merge the Strategic Value Portfolio and the total performance of the Value Portfolio and the difference
#is the value add from tactical shifts

Value_Portfolio <- merge(Total_Performance_value,Total_Performance_value_total)
Value_Portfolio$Tactical_Value_Add <- Value_Portfolio$portfolio.returns.Total_Performance_value_total - 
                                      Value_Portfolio$portfolio.returns.Total_Performance_value

colnames(Value_Portfolio) <- c("Strategic Value Performance", "Total Value Performance", "Tactical Benefit")

chart.CumReturns(Value_Portfolio, legend.loc = "bottomleft")

####################################################################################################

#calculate benefit or cost of adding technical indicators to the portfolio by seeing how pure technical 
#portfolio with a base 100% allocation vs. equal weight performed

January_weights_technical <- c(#IWB
  .13,
  #IEV
  .13,
  #EWJ
  .23,
  #EPP
  .02,
  #VWO
  .14,
  #ILF
  .08,
  #FXI
  .12,
  #RWO
  -.01,
  #GSG
  .10,
  #GLD
  .02,
  #SHY
  .00,
  #IEF
  -.02,
  #TLT
  .00,
  #BWX
  .02,
  #LQD
  -.01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .03,
  #TIP
  .00,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)


sum(January_weights_technical) #confirm the weights add up to 1
length(January_weights_technical) #confirm the weights are 24 and the same length as number of columns in MinCorrReturns

#create portfolio return
January_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = January_weights_technical, rebalance_on = "months")

#trim the data to be just for the month analyzed
January_Performance_technical <- January_Performance_technical[January_Rebalance_Dates]

February_weights_technical <- c(#IWB
  .30,
  #IEV
  .09,
  #EWJ
  .16,
  #EPP
  .00,
  #VWO
  .16,
  #ILF
  .18,
  #FXI
  .17,
  #RWO
  -.04,
  #GSG
  .15,
  #GLD
  .02,
  #SHY
  .00,
  #IEF
  -.04,
  #TLT
  -.04,
  #BWX
  .02,
  #LQD
  -.04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  -.03,
  #EMB
  -.03,
  #TIP
  -.03,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(February_weights_technical)
length(February_weights_technical)
February_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = February_weights_technical, rebalance_on = "months")
February_Performance_technical <- February_Performance_technical[February_Rebalance_Dates]


March_weights_technical <- c(#IWB
  .11,
  #IEV
  .07,
  #EWJ
  .23,
  #EPP
  .01,
  #VWO
  .13,
  #ILF
  .19,
  #FXI
  .15,
  #RWO
  -.02,
  #GSG
  .28,
  #GLD
  .01,
  #SHY
  .00,
  #IEF
  -.02,
  #TLT
  -.03,
  #BWX
  .01,
  #LQD
  -.03,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  -.04,
  #EMB
  -.03,
  #TIP
  -.02,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(March_weights_technical)
length(March_weights_technical)
March_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = March_weights_technical, rebalance_on = "months")
March_Performance_technical <- March_Performance_technical[March_Rebalance_Dates]

April_weights_technical <- c(#IWB
  -.02,
  #IEV
  .05,
  #EWJ
  .19,
  #EPP
  -.01,
  #VWO
  .15,
  #ILF
  .19,
  #FXI
  .15,
  #RWO
  .00,
  #GSG
  .20,
  #GLD
  .06,
  #SHY
  .00,
  #IEF
  -.01,
  #TLT
  -.01,
  #BWX
  .13,
  #LQD
  -.01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  -.04,
  #EMB
  -.01,
  #TIP
  -.01,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(April_weights_technical)
length(April_weights_technical)
April_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = April_weights_technical, rebalance_on = "months")
April_Performance_technical <- April_Performance_technical[April_Rebalance_Dates]

May_weights_technical <- c(#IWB
  .07,
  #IEV
  .01,
  #EWJ
  .16,
  #EPP
  .02,
  #VWO
  .02,
  #ILF
  .09,
  #FXI
  .07,
  #RWO
  .02,
  #GSG
  .24,
  #GLD
  -.01,
  #SHY
  .23,
  #IEF
  -.02,
  #TLT
  .00,
  #BWX
  .12,
  #LQD
  -.02,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  -.01,
  #EMB
  -.02,
  #TIP
  .03,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(May_weights_technical)
length(May_weights_technical)
May_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = May_weights_technical, rebalance_on = "months")
May_Performance_technical <- May_Performance_technical[May_Rebalance_Dates]

June_weights_technical <- c(#IWB
  .19,
  #IEV
  .01,
  #EWJ
  .13,
  #EPP
  .16,
  #VWO
  -.02,
  #ILF
  -.03,
  #FXI
  .12,
  #RWO
  .06,
  #GSG
  .23,
  #GLD
  .07,
  #SHY
  .00,
  #IEF
  -.01,
  #TLT
  .03,
  #BWX
  -.03,
  #LQD
  -.02,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  -.03,
  #TIP
  .12,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(June_weights_technical)
length(June_weights_technical)
June_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = June_weights_technical, rebalance_on = "months")
June_Performance_technical <- June_Performance_technical[June_Rebalance_Dates]


July_weights_technical <- c(#IWB
  .26,
  #IEV
  -.04,
  #EWJ
  .12,
  #EPP
  .14,
  #VWO
  -.03,
  #ILF
  -.01,
  #FXI
  .00,
  #RWO
  .16,
  #GSG
  .19,
  #GLD
  -.04,
  #SHY
  .15,
  #IEF
  .01,
  #TLT
  .04,
  #BWX
  -.01,
  #LQD
  .01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .02,
  #EMB
  .02,
  #TIP
  .01,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(July_weights_technical)
length(July_weights_technical)
July_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = July_weights_technical, rebalance_on = "months")
July_Performance_technical <- July_Performance_technical[July_Rebalance_Dates]

August_weights_technical <- c(#IWB
  .25,
  #IEV
  .03,
  #EWJ
  .06,
  #EPP
  .08,
  #VWO
  -.01,
  #ILF
  -.01,
  #FXI
  .00,
  #RWO
  .14,
  #GSG
  .13,
  #GLD
  -.04,
  #SHY
  .00,
  #IEF
  .01,
  #TLT
  -.01,
  #BWX
  -.04,
  #LQD
  .10,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .20,
  #EMB
  .02,
  #TIP
  .09,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(August_weights_technical)
length(August_weights_technical)
August_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = August_weights_technical, rebalance_on = "months")
August_Performance_technical <- August_Performance_technical[August_Rebalance_Dates]

September_weights_technical <- c(#IWB
  .20,
  #IEV
  -.02,
  #EWJ
  .03,
  #EPP
  .03,
  #VWO
  -.02,
  #ILF
  -.01,
  #FXI
  -.01,
  #RWO
  .22,
  #GSG
  .11,
  #GLD
  -.01,
  #SHY
  .00,
  #IEF
  .07,
  #TLT
  .00,
  #BWX
  .00,
  #LQD
  .05,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .40,
  #EMB
  -.02,
  #TIP
  -.02,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(September_weights_technical)
length(September_weights_technical)
September_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = September_weights_technical, rebalance_on = "months")
September_Performance_technical <- September_Performance_technical[September_Rebalance_Dates]


October_weights_technical <- c(#IWB
  .21,
  #IEV
  -.02,
  #EWJ
  .14,
  #EPP
  .01,
  #VWO
  -.03,
  #ILF
  .02,
  #FXI
  -.01,
  #RWO
  .11,
  #GSG
  .17,
  #GLD
  -.01,
  #SHY
  .13,
  #IEF
  -.04,
  #TLT
  -.05,
  #BWX
  -.01,
  #LQD
  -.04,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .47,
  #EMB
  -.01,
  #TIP
  -.04,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(October_weights_technical)
length(October_weights_technical)
October_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = October_weights_technical, rebalance_on = "months")
October_Performance_technical <- October_Performance_technical[October_Rebalance_Dates]

November_weights_technical <- c(#IWB
  .02,
  #IEV
  -.03,
  #EWJ
  .01,
  #EPP
  -.03,
  #VWO
  -.04,
  #ILF
  .03,
  #FXI
  -.01,
  #RWO
  .04,
  #GSG
  .00,
  #GLD
  .01,
  #SHY
  .93,
  #IEF
  -.06,
  #TLT
  -.05,
  #BWX
  -.02,
  #LQD
  -.06,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  .34,
  #EMB
  -.02,
  #TIP
   -.06,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(November_weights_technical)
length(November_weights_technical)
November_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = November_weights_technical, rebalance_on = "months")
November_Performance_technical <-November_Performance_technical[November_Rebalance_Dates]

December_weights_technical <- c(#IWB
  .02,
  #IEV
  -.02,
  #EWJ
  .02,
  #EPP
  -.01,
  #VWO
  .00,
  #ILF
  .00,
  #FXI
  -.01,
  #RWO
  .05,
  #GSG
  -.04,
  #GLD
  .03,
  #SHY
  .88,
  #IEF
  .11,
  #TLT
  .03,
  #BWX
  .01,
  #LQD
  -.01,
  #FLOT
  .00,
  #BKLN
  .00,
  #HYG
  -.03,
  #EMB
  -.03,
  #TIP
  .00,
  #HDGE
  .00,
  #TAIL
  .00,
  #QAI
  .00,
  #DIVY
  .00)

sum(December_weights_technical)
length(December_weights_technical)
December_Performance_technical <- Return.portfolio(MinCorr_ETFs_Returns, weights = December_weights_technical, rebalance_on = "months")
December_Performance_technical <-December_Performance_technical[December_Rebalance_Dates]

#Rbind all of the performance months
Total_Performance_technical <- rbind.zoo(January_Performance_technical,
                                           February_Performance_technical,
                                           March_Performance_technical,
                                           April_Performance_technical,
                                           May_Performance_technical,
                                           June_Performance_technical,
                                           July_Performance_technical,
                                           August_Performance_technical,
                                           September_Performance_technical,
                                           October_Performance_technical,
                                           November_Performance_technical,
                                           December_Performance_technical)


#combine MinCorr data (may need to import) and total technical/momentum combined adjustment
#then subtract to see if technicals added/subtracted value

#import MinCorr Data from Performance Folder in GitHub


Momentum_Technical_Performance <- merge(MinCorr_Performance_xts["2018"],Total_Performance_technical)
Momentum_Technical_Performance$Technical_alpha <- Momentum_Technical_Performance[,2] - Momentum_Technical_Performance[,1]
colnames(Momentum_Technical_Performance) <- c("MinCorr Performance",
                                              "Mincorr Plus Technical Adjustments Performance",
                                              "Alpha from Technical Adjustments")

par(mfrow = c(2,1))

chart.CumReturns(Momentum_Technical_Performance, legend.loc = "topright", main = "Momentum and Trend Components")

chart.CumReturns(Value_Portfolio, legend.loc = "bottomleft")

#################################DONE WITH CALCULATIONS FOR PORTFOLIO####################################

#open up script and run "Calculating Risk Models" under client folder to get risk 6 model

#opens the script
file.edit("/Users/downey/Dropbox/Holborn Assets/Coding/R/Images and How to do functions/Charts for Client Meetings/Calculating the Risk Model Returns to Use in Performance Summary.R")

#runs the whole script
source("/Users/downey/Dropbox/Holborn Assets/Coding/R/Images and How to do functions/Charts for Client Meetings/Calculating the Risk Model Returns to Use in Performance Summary.R")


chart.CumReturns(Risk6returns["2018"])

Risk6vsBenchmark <- merge(Total_Performance1,Risk6returns["2018"])
Risk6vsBenchmark$alpha <- Risk6vsBenchmark[,1] - Risk6vsBenchmark[,2]
colnames(Risk6vsBenchmark) <- c("Risk 6 Discretionary", "Risk 6 Model", "Alpha")

chart.CumReturns(Risk6vsBenchmark, legend.loc = "bottomleft")

RiskFreeReturn <- table.AnnualizedReturns(Return.calculate(SHY$SHY.Adjusted["2018"]))
RiskFreeReturn <- RiskFreeReturn[1,]

Fund_stats_returns <- table.AnnualizedReturns(Risk6vsBenchmark, Rf = RiskFreeReturn/252)
Fund_stats_Sortino <- SortinoRatio(Risk6vsBenchmark)
Fund_stats_Calmar <- CalmarRatio(Risk6vsBenchmark)
Fund_stats_MaxDD <- maxDrawdown(Risk6vsBenchmark)
Fund_stats_Kurtosis <- PerformanceAnalytics::kurtosis(Risk6vsBenchmark)
Fund_stats_skewness <- PerformanceAnalytics::skewness(Risk6vsBenchmark)
Fund_stats_skewnesskurtosis <- SkewnessKurtosisRatio(Risk6vsBenchmark)
Fund_stats_Ulcerindex <- UlcerIndex(Risk6vsBenchmark)
Fund_stats_VaR <- PerformanceAnalytics::VaR(Risk6vsBenchmark, p = .95, method = "historical")
Fund_stats_ES <- PerformanceAnalytics::ES(Risk6vsBenchmark, p = .95, method = "historical")

Fund_Stats <- rbind(Fund_stats_returns,
                    Fund_stats_Sortino,
                    Fund_stats_Calmar,
                    Fund_stats_MaxDD,
                    Fund_stats_Kurtosis,
                    Fund_stats_skewness,
                    Fund_stats_skewnesskurtosis,
                    Fund_stats_VaR,
                    Fund_stats_ES,
                    Fund_stats_Ulcerindex)

Table_Stats <- textplot(round(Fund_Stats, digits = 3), wrap = FALSE)
title(main = "Fund Statistics since January 1 2018 (Daily)")

Risk6vsBenchmark

autoplot(rollapply(Risk6vsBenchmark[,c(1,2)],
                   FUN = StdDev.annualized, scale = 252,
                   width = 20), facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Rolling Weekly Volatility (Annualized)",
       x = "Date", y = "Standard Deviation %")

#performance decomposition with MinCorr, Value, and Technical Side of things

#Import the MinCorr Performance from Working Directory > Portolios

par(mfrow = c(2,1))

chart.CumReturns(MinCorr_Performance_xts["2018"])
chart.CumReturns(merge(Risk6vsBenchmark), legend.loc = "bottomleft")
  


###################################################################################

#Getting the annualized return and risk for mincorr symbols

MinCorr_Symbols <- c("EPP",
                     "IEV",
                     "IWB",
                     "EWJ",
                     "VWO",
                     "ILF",
                     "RWO",
                     "GLD",
                     "GSG",
                     "IEF",
                     "SHY",
                     "BWX",
                     "TIP",
                     "LQD",
                     "EMB",
                     "HYG",
                     "TLT",
                     "FXI",
                     "BKLN",
                     "FLOT",
                     "GCC")

Alternative_ETFs_MFs <- c("HDGE",
                          "TAIL",
                          "GMOM",
                          "QAI",
                          "DIVY",
                          "AMFAX",
                          "MFTNX")

#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols(MinCorr_Symbols, src = "av", 
           output.size = "full", adjusted = TRUE)
getSymbols(Alternative_ETFs_MFs, src = "av", 
           output.size = "full", adjusted = TRUE)

#Choose the date you want to use to start the analysis
Start_Date <- "2018"

Global_ETFs_Merged <- merge(EPP,
                                 IEV,
                                 IWB,
                                 EWJ,
                                 VWO,
                                 ILF,
                                 RWO,
                                 GLD,
                                 GSG,
                                 IEF,
                                 SHY,
                                 BWX,
                                 TIP,
                                 LQD,
                                 EMB,
                                 HYG,
                                 TLT,
                                 FXI,
                                 BKLN,
                                 FLOT,
                                 GCC)

Alternative_ETFs_MFs_merged <- merge(GMOM,AMFAX,DIVY,
                                    HDGE,MFTNX,QAI,TAIL)

#truncate data to start from beginning of the year

Global_ETFs_Merged <- Global_ETFs_Merged[Start_Date]
Alternative_ETFs_MFs_merged <- Alternative_ETFs_MFs_merged[Start_Date]

#calculate returns
Global_ETFs_Merged_Returns <- Return.calculate(Global_ETFs_Merged[,seq(6,ncol(Global_ETFs_Merged),6)])

Alternative_ETFs_MFs_merged_returns <- Return.calculate(Alternative_ETFs_MFs_merged[,seq(6,ncol(Alternative_ETFs_MFs_merged),6)])


#rename the columns to be more intuitive with ticker symbols
colnames(Global_ETFs_Merged_Returns) <- c("Pacific ex-Japan Equity (EPP)",
                               "Japan Equity (EWJ)",
                               "China Equity (FXI)",
                               "Gold (GLD)",
                               "Commodities (GSG)",
                               "USA High Yield (HYG)",
                               "USA 7-10 Year Treasuries (IEF)",
                               "Developed Europe Equity (IEV)",
                               "Latin America Equity (ILF)",
                               "USA Large Cap Equity (IWB)",
                               "USA Inv Grade Bonds (LQD)",
                               "Global Real Estate (RWO)",
                               "USA TIPS (TIP)",
                               "USA 20+ Years Treasuries (TLT)",
                               "Emerging Market Equity (VWO)",
                               "Emerging Market Bonds (EMB)",
                               "Int'l Treasury Bonds (BWX)",
                               "USA 1-3 Year Treasuries (SHY)",
                               "High Yield Floating Sen. Loans (BKLN)",
                               "Investment Grade Floating Rate (FLOT)",
                               "Commodities Equal Weight (GCC)")

colnames(Alternative_ETFs_MFs_merged_returns) <- c("Cambria Global Momentum (GMOM)",
                                 "Natixis Managed Futures (AMFAX)",
                                 "Realtyshares DIVY (DIVY)",
                                 "Ranger Dedicated Short (HDGE)",
                                 "Arrow_DUNN Trend (MFTNX)",
                                 "IQ Hedge Multi-Strategy (QAI)",
                                 "Cambria Tail Risk (TAIL)")


#this table will give us an overview of performance and risk for the year for the funds

par(mfrow = c(4,1))

textplot(table.AnnualizedReturns(Global_ETFs_Merged_Returns[,c(1:7)]))
textplot(table.AnnualizedReturns(Global_ETFs_Merged_Returns[,c(8:14)]))
textplot(table.AnnualizedReturns(Global_ETFs_Merged_Returns[,c(15:21)]))
textplot(table.AnnualizedReturns(Alternative_ETFs_MFs_merged_returns))

end_t<-Sys.time()

print(end_t-start_t)


