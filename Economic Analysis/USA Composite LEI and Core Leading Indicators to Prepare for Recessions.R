library(PerformanceAnalytics)
library(quantmod)
library(tidyverse)
library(Quandl)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)
library(TTR)

#create custome ggplot theme
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

#choose start and end dates
start <- as.Date("1900-01-01")
end <- Sys.Date()

#set api key
Quandl.api_key("MFiHkkmpYSxDhfZ1ygrU")

#set the indices to download
Leading_Indicator_Symbols <- c("FRED/NEWORDER",
                               "FRED/PERMIT",
                               "YALE/SPCOMP.1",
                               "FRED/T10Y3M",
                               "FRED/USSLIND",
                               "FRED/RECPROUSM156N")

Yale <- Quandl("YALE/SPCOMP", api_key="MFiHkkmpYSxDhfZ1ygrU", type = "xts")


#Data for OECD Composite Leading Indicator and GDP Trend
GDPTrend <-Quandl("OECD/MEI_CLI_LORSGPRT_USA_M", 
                  api_key="MFiHkkmpYSxDhfZ1ygrU",
                  type = "xts")

OECD_LEI <- Quandl("OECD/KEI_LOLITOAA_USA_ST_M", 
                   api_key="MFiHkkmpYSxDhfZ1ygrU", 
                   start_date="1955-01-31")

OECD_LEI_xts <- as.xts(OECD_LEI[,2], order.by = OECD_LEI$Date)
colnames(OECD_LEI_xts) <- "OECD Composite Leading Index"

#download the symbols
Leading_Economic_Indicators <- Quandl(Leading_Indicator_Symbols, type = "xts",
                start_date="1900-01-01", end_date= Sys.Date(),
                collapse = "monthly")

#rename the column names to be intuitive
colnames(Leading_Economic_Indicators) <- 
  c("Manuf. New Orders: Nondefense Cap. Goods Ex-Aircraft",
    "Building permits, new private housing units",
    "S&P Composite",
    "10 Year Treasury - 3 Month T Bill",
    "FRED LEI for USA",
    "Smoothed Recession Probabilities")

#create 12 month SMA of Building Permits as a leading indicator
BuildingPermits <- na.omit(Leading_Economic_Indicators$`Building permits, new private housing units`)
BuildingPermits$SMA12 <- SMA(BuildingPermits, n = 12)
tail(BuildingPermits)

colnames(BuildingPermits) <- c("Building Permits",
                            "12-Month Moving Average Permits")

#create 12 month SMA of Earnings as a leading indicator
SP_Earnings <- na.omit(Yale$Earnings)
SP_Earnings$SMA12 <- SMA(SP_Earnings, n = 12)
tail(SP_Earnings, 60)
SP_Earnings$Year.on.Year.Change <- ROC(SP_Earnings[,1], n = 12)
head(SP_Earnings)
colnames(SP_Earnings) <- c("S&P 500 Earnings",
                               "12-Month Moving Average Earnings",
                           "Year on Year Change")

#create 12 month SMA of SP 500 as a Trend Indicators
SP500 <- na.omit(Leading_Economic_Indicators$`S&P Composite`)
SP500$SMA12 <- SMA(SP500, n = 12)
tail(SP500)

colnames(SP500) <- c("SP Composite",
                               "12-Month Moving Average SP Composite")

#create 12 month SMA of Manufacturer's New Orders
NewOrders <- na.omit(Leading_Economic_Indicators$`Manuf. New Orders: Nondefense Cap. Goods Ex-Aircraft`)
NewOrders$SMA12 <- SMA(NewOrders, n = 12)
tail(NewOrders)

colnames(NewOrders) <- c("Manuf.NewOrders.Nondefense.CapGoods.exAircraft",
                     "12-Month Moving Average Manuf. New Orders")

#create 12 month SMA of FED LEI
FEDLEI <- na.omit(Leading_Economic_Indicators$`FRED LEI for USA`)
FEDLEI$SMA12 <- SMA(FEDLEI, n = 12)
tail(FEDLEI)

colnames(FEDLEI) <- c("FRED LEI for USA",
                         "12-Month Moving Average FRED LEI for USA")


#choose a start date for the graphs
START_DATE <- "2006/"

#create each graph
LE1 <- autoplot(merge(Leading_Economic_Indicators[,1], NewOrders[,2])[START_DATE], 
                facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Manufacturer's New Orders: Nondefense Cap. Goods Ex-Aircraft", 
       x = "Date", y = "New Orders") +
  theme(legend.position="none")

LE2 <- autoplot(merge(Leading_Economic_Indicators[,2],BuildingPermits[,2])[START_DATE],
                facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 8, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Building permits, new private housing units, seasonally adjusted",
       x = "Date", y = "Thousands") +
  theme(legend.position="none")

LE3 <- autoplot(merge(Leading_Economic_Indicators[,3],SP500[,2])[START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="S&P Composite", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

LE4 <- autoplot(Leading_Economic_Indicators[,4][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="10 Year Treasury - 3 Month T Bill", 
       x = "Date", y = "Interest Rate Spread")

LE5 <- autoplot(merge(GDPTrend,OECD_LEI_xts)[START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="OECD USA LEI and GDP Trend",
       subtitle = "100 equals long term trend and deviations indicate slowing/expanding growth; LEI aims to anticipate by 4-6 months",
       x = "Date", y = "Index")

LE6 <- autoplot(merge(Leading_Economic_Indicators[,5],FEDLEI[,2])[START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Leading Economic Index for USA", 
       x = "Date", y = "Index", caption = "Source: Quandle, OECD, Philadelphia Fed, St. Louis Fed, ISM") + 
  theme(legend.position="none")

LE7 <- autoplot(Leading_Economic_Indicators[,6]["1969/"]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="Smoothed Recession Probabilities", 
       subtitle = "Three consecutive months of smoothed probabilities above 80% has been a reliable signal of the start of a new recession, \nwhile three consecutive months of smoothed probabilities below 20% has been a reliable signal of the start of a new expansion",
       x = "Date", y = "Probabilities (%)", caption = "Source: St. Louis Fed")

LE8 <- autoplot(SP_Earnings[,c(1,2)][START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="S&P 500 Earnings (Red Data; Green 12 Mo SMA)", 
       x = "Date", y = "Earnings", caption = "Source: Yale - Robert Shiller")

LE9 <- autoplot(SP_Earnings[,3]["2014/"], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="S&P 500 Earnings Change Year on Year", 
       x = "Date", y = "Percentage Change", caption = "Source: Yale - Robert Shiller")


#combine all 9 graphs
grid.arrange(
 LE1, LE2, LE3, LE4, LE5, LE6,
  nrow = 3, ncol = 2,
  top = "Favorite Leading Indicators (Red Line Indicator and Green 12 Mo SMA except where noted)"
)

LE7
LE8
LE9

