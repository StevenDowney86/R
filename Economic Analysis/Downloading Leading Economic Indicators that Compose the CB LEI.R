library(PerformanceAnalytics)
library(quantmod)
library(tidyverse)
library(Quandl)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)

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
Leading_Indicator_Symbols <- c("FRED/AWHMAN",
                               "FRED/IC4WSA",
                               "FRED/ACDGNO",
                               "ISM/MAN_NEWORDERS.5",
                               "FRED/NEWORDER",
                               "FRED/PERMIT",
                               "YALE/SPCOMP.1",
                               "UMICH/SOC1",
                               "FRED/T10Y3M")

#download the symbols
Leading_Economic_Indicators <- Quandl(Leading_Indicator_Symbols, type = "xts",
                start_date="1900-01-01", end_date= Sys.Date(),
                collapse = "monthly")

#rename the column names to be intuitive
colnames(Leading_Economic_Indicators) <- 
  c("Average weekly hours, manufacturing",
    "Avg. 4 week initial claims for unemployment insurance, seasonally adj.",
     "Manuf.' new orders, consu. goods and materials",
    "Manufacturing New Orders Index",
    "Manuf. New Orders: Nondefense Cap. Goods Ex-Aircraft",
    "Building permits, new private housing units",
    "S&P Composite",
    "Univ. Michigan Consumer Sentiment",
    "10 Year Treasury - 3 Month T Bill")

#create 12 month SMA of Building Permits as a leading indicator
BuildingPermits <- na.omit(Leading_Economic_Indicators$`Building permits, new private housing units`)
BuildingPermits$SMA12 <- SMA(BuildingPermits, n = 12)
tail(BuildingPermits)

colnames(BuildingPermits) <- c("Building Permits",
                            "12-Month Moving Average Permits")


#choose a start date for the graphs
START_DATE <- "2014/"

#create each graph
LE1 <- autoplot(Leading_Economic_Indicators[,1][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Average weekly hours, manufacturing", 
       x = "Date", y = "Hours")

LE2 <- autoplot(Leading_Economic_Indicators[,2][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 8, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Avg. 4 week init. claims for unempl.insur. (seasonally adjusted)", 
       x = "Date", y = "Initial Claims")

LE3 <- autoplot(Leading_Economic_Indicators[,3][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Manufacturer's new orders, consumer goods and materials", 
       x = "Date", y = "Order Number")

LE4 <- autoplot(Leading_Economic_Indicators[,4][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Manufacturing New Orders Index", 
       x = "Date", y = "Index")

LE5 <- autoplot(Leading_Economic_Indicators[,5][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Manufacturer's New Orders: Nondefense Cap. Goods Ex-Aircraft", 
       x = "Date", y = "New Orders")

LE6 <- autoplot(merge(Leading_Economic_Indicators[,6],BuildingPermits[,2])[START_DATE],
                facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 8, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Building permits, new private housing units, seasonally adjusted",
       x = "Date", y = "Thousands") +
  theme(legend.position="none")

LE7 <- autoplot(Leading_Economic_Indicators[,7][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="S&P Composite", 
       x = "Date", y = "Index")

LE8 <- autoplot(Leading_Economic_Indicators[,8][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Univeristy of Michigan Consumer Sentiment", 
       x = "Date", y = "Index")

LE9 <- autoplot(Leading_Economic_Indicators[,9][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="10 Year Treasury - 3 Month T Bill", 
       x = "Date", y = "Interest Rate Spread", caption = "St. Louis Fed, ISM")

#combine all 9 graphs
grid.arrange(
 LE1, LE2, LE3, LE4, LE5, LE6, LE7, LE8, LE9,
  nrow = 3, ncol = 3,
  top = "Conference Board Leading Index Components (Monthly)"
)


