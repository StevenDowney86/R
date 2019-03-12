library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)

#this script will download the SPDR Select Sector ETFs
#from alphavantage and calculate their returns YTD, 
#and will show them in different charts.

Market_Cap_ETFs <- c("MGC",
                      "IWB",
                     "IWR",
                      "IWM",
                      "IWC")


#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols(Market_Cap_ETFs, src = "av", 
           output.size = "full", adjusted = TRUE)

#Choose the date you want to use to start the analysis
Start_Date <- "2018-01-01/"

#truncate data to start from beginning of the year
MGC <- MGC[Start_Date]
IWB <- IWB[Start_Date]
IWR <- IWR[Start_Date]
IWM <- IWM[Start_Date]
IWC <- IWC[Start_Date]


head(MGC)

#calculate returns
MGC$Returns <- Return.calculate(MGC$MGC.Adjusted)
IWB$Returns <- Return.calculate(IWB$IWB.Adjusted)
IWR$Returns <- Return.calculate(IWR$IWR.Adjusted)
IWM$Returns <- Return.calculate(IWM$IWM.Adjusted)
IWC$Returns <- Return.calculate(IWC$IWC.Adjusted)


#Replace NA's with 0 as it is the first return point
MGC[is.na(MGC)] <- 0
IWB[is.na(IWB)] <- 0
IWR[is.na(IWR)] <- 0
IWM[is.na(IWM)] <- 0
IWC[is.na(IWC)] <- 0


#Calculate cumulative growth
MGC$MGCCumGrowth <- cumprod(1 + (MGC$Returns)) - 1
IWB$IWBCumGrowth <- cumprod(1 + (IWB$Returns)) - 1
IWR$IWRCumGrowth <- cumprod(1 + (IWR$Returns)) - 1
IWM$IWMCumGrowth <- cumprod(1 + (IWM$Returns)) - 1
IWC$IWCCumGrowth <- cumprod(1 + (IWC$Returns)) - 1


#combine all of the symbols from Mincorr
Market_Cap_ETFs_DATA <- merge(MGC,
                                 IWB,
                                IWR,  
                                 IWM,
                                 IWC)
                               
              
#Merge only the cumulative growth columns to use in ggplot
Market_Cap_ETFs_DATA_CumGrowth <- Market_Cap_ETFs_DATA[,c(seq(8,40,8))]

#Merge only the returns for each sector ETF
Market_Cap_ETFs_DATA_Returns <- Market_Cap_ETFs_DATA[,c(seq(7,39,8))]


#rename the columns to be more intuitive with ticker symbols
colnames(Market_Cap_ETFs_DATA_CumGrowth) <- c("Mega Cap (MGC)",
                               "Large Cap (IWB)",
                               "Mid Cap (IWR)",
                               "Small-Cap (IWM)",
                               "Micro-Cap (IWC)"
                               )
colnames(Market_Cap_ETFs_DATA_Returns) <- c("Mega Cap (MGC)",
                                              "Large Cap (IWB)",
                                            "Mid Cap (IWR)",
                                              "Small-Cap (IWM)",
                                              "Micro-Cap (IWC)"
)


#creat ggplot in autoplot
Market_Cap_Graph <- autoplot(Market_Cap_ETFs_DATA_CumGrowth, facets = NULL) +
  theme_economist() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="US Equity by Market Cap Total Return Performance",
       x = "Date", y = "Percent Growth", caption = "Source: Alphavantage")

Market_Cap_Graph

chart.Drawdown(Market_Cap_ETFs_DATA_Returns[Start_Date], 
               legend.loc = "bottomleft",
               main = "Sector Drawdowns")

