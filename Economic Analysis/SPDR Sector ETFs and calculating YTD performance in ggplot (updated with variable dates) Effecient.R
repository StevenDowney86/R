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

SPDR_Sector_ETFs <- c("XLY",
                      "XLRE",
                      "XLI",
                      "XLE",
                      "XLC",
                      "XLV",
                      "XLF",
                      "XLP",
                      "XLB")


#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols(SPDR_Sector_ETFs, src = "av", 
           output.size = "full", adjusted = TRUE)

#Choose the date you want to use to start the analysis
Start_Date <- "2018/"

SPDR_Sector_ETFs_Merged <- merge(XLY,
                                 XLRE,
                                 XLI,
                                 XLE,
                                 XLC,
                                 XLV,
                                 XLF,
                                 XLP,
                                 XLB)

#truncate data to start from beginning of the year
SPDR_Sector_ETFs_Merged <- SPDR_Sector_ETFs_Merged[Start_Date]

#calculate returns
SPDR_Sector_ETFs_Merged_Returns <- Return.calculate(SPDR_Sector_ETFs_Merged[,seq(6,ncol(SPDR_Sector_ETFs_Merged),6)])

head(SPDR_Sector_ETFs_Merged_Returns)
#Replace NA's with 0 as it is the first return point
SPDR_Sector_ETFs_Merged_Returns[is.na(SPDR_Sector_ETFs_Merged_Returns)] <- 0
XLRE[is.na(XLRE)] <- 0



#Calculate cumulative growth
SPDR_Sector_ETFs_Merged_CumGrowth <- cumprod(1 + (SPDR_Sector_ETFs_Merged_Returns)) - 1

head(SPDR_Sector_ETFs_Merged_CumGrowth)

#rename the columns to be more intuitive with ticker symbols
colnames(SPDR_Sector_ETFs_Merged_CumGrowth) <- c("Consumer Discretionary (XLY)",
                               "Real Estate (XLRE)",
                               "Industrial (XLI)",
                               "Energy (XLE)",
                               "Communication Services (XLC)",
                               "Health Care (XLV)",
                               "Financials (XLF)",
                               "Consumer Staples (XLP)",
                               "Materials (XLB)")

colnames(SPDR_Sector_ETFs_Merged_Returns) <- c("Consumer Discretionary (XLY)",
                                                 "Real Estate (XLRE)",
                                                 "Industrial (XLI)",
                                                 "Energy (XLE)",
                                                 "Communication Services (XLC)",
                                                 "Health Care (XLV)",
                                                 "Financials (XLF)",
                                                 "Consumer Staples (XLP)",
                                                 "Materials (XLB)")

#creat ggplot in autoplot
SPDR_Sector_Graph <- autoplot(SPDR_Sector_ETFs_Merged_CumGrowth, facets = NULL) +
  theme_economist() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="US Large Cap Sector Total Return Performance",
       x = "Date", y = "Percent Growth", caption = "Source: Alphavantage")

SPDR_Sector_Graph

chart.Drawdown(SPDR_Sector_ETFs_Merged_Returns[Start_Date], 
               legend.loc = "bottomleft",
               main = "Sector Drawdowns")

textplot(round(maxDrawdown(SPDR_Sector_ETFs_Merged_Returns),3))

         