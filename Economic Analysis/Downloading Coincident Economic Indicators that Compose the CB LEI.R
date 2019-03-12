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
Coincident_Indicator_Symbols <- c("FRED/PAYEMS",
                               "FRED/UNRATE",
                               "FRED/AWHMAN",
                               "FRED/CES0500000003",
                               "FRED/INDPRO",
                               "FRED/CMRMTSPL")
                              

#download the symbols
Coincident_Economic_Indicators <- Quandl(Coincident_Indicator_Symbols, type = "xts",
                start_date="1900-01-01", end_date= Sys.Date(),
                collapse = "monthly")

#rename the column names to be intuitive
colnames(Coincident_Economic_Indicators) <- 
  c("All Employees: Total Nonfarm Payrolls",
    "Civilian Unemployment Rate",
    "Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing",
    "Average Hourly Earnings of All Employees: Total Private",
    "Industrial Production Index",
    "Real Manufacturing and Trade Sales"
   )

#create 12 month SMA of unemployment rate per Jesse Livermore at
#Philisophical Economics as a prime coincident indicator
Unemployment <- Coincident_Economic_Indicators$`Civilian Unemployment Rate`
Unemployment$TwelveMOSMA <- SMA(Unemployment$`Civilian Unemployment Rate`, n = 12)
tail(Unemployment)

colnames(Unemployment) <- c("Civilian Unemployment",
                            "12-Month Moving Average Unemp.")
  

#choose a start date for the graphs
START_DATE <- "2014/"

#create each graph
G1 <- autoplot(Coincident_Economic_Indicators[,1][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Total Nonfarm Payrolls", 
       x = "Date", y = "Thousands")

G2 <- autoplot(merge(Coincident_Economic_Indicators[,2],Unemployment[,2])
               [START_DATE],
               facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Civilian Unemployment Rate", 
       x = "Date", y = "Percent") +
  theme(legend.position="none")

G3 <- autoplot(Coincident_Economic_Indicators[,3][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Average Weekly Hours of Production: Manufacturing", 
       x = "Date", y = "Hours")

G4 <- autoplot(Coincident_Economic_Indicators[,4][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Average Hourly Earnings of All Employees: Total Private", 
       x = "Date", y = "Earnings")

G5 <- autoplot(Coincident_Economic_Indicators[,5][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Industrial Production Index", 
       x = "Date", y = "Index")

G6 <- autoplot(Coincident_Economic_Indicators[,6][START_DATE]) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Real Manufacturing and Trade Sales",
       x = "Date", y = "Millions")

#combine all 6 graphs
grid.arrange(
 G1, G2, G3, G4, G5, G6,
  nrow = 2, ncol = 3,
  top = "Conference Board and FRED Coincident Index Components (Monthly)"
)


