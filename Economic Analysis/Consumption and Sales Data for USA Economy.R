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
Sales_Symbols <- c("FRED/RSXFS",
                               "FRED/TOTALSA",
                               "FRED/HTRUCKSSAAR",
                   "FRED/DPCERAM1M225NBEA")

#download the symbols
Sales_Indicators <- Quandl(Sales_Symbols, type = "xts",
                start_date="1900-01-01", end_date= Sys.Date(),
                collapse = "monthly")

#rename the column names to be intuitive
colnames(Sales_Indicators) <- 
  c("Advanced Retail Trade",
    "Total Vehicle Sales",
     "Motor Vehicle Retail Sales: Heavy Weight Trucks"
    "Real Personal Consumption Expenditures")

#create 12 month SMA of Building Permits as a leading indicator
BuildingPermits <- na.omit(Leading_Economic_Indicators$`Building permits, new private housing units`)
BuildingPermits$SMA12 <- SMA(BuildingPermits, n = 12)
tail(BuildingPermits)

colnames(BuildingPermits) <- c("Building Permits",
                            "12-Month Moving Average Permits")


#choose a start date for the graphs
START_DATE <- "2014/"

#create each graph
LE1 <- autoplot(Sales_Indicators[START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Average weekly hours, manufacturing", 
       x = "Date", y = "Hours")

LE1

#combine all 9 graphs
grid.arrange(
 LE1, LE2, LE3, LE4, LE5, LE6, LE7, LE8, LE9,
  nrow = 3, ncol = 3,
  top = "Conference Board Leading Index Components (Monthly)"
)


