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

#get wage and employment details
Wage_Symbols <- c("FRED/PAYEMS",
                  "FRED/AWHMAN",
                  "FRED/CES0500000003")

#download the symbols
Wages_Employment <- Quandl(Wage_Symbols, type = "xts",
                                      start_date="1900-01-01", end_date= Sys.Date(),
                                      collapse = "monthly")
Wages_Employment <- na.omit(Wages_Employment)
Wages_Employment <- Wages_Employment["2000/"]

colnames(Wages_Employment) <- c("Total Employed Thousands",
                                "Median Weekly Work Hours",
                                "Median Hourly Wage")
Wages_Employment$Cumulative_Wages <- Wages_Employment$`Total Employed Thousands` * 1000 *
                                      Wages_Employment$`Median Weekly Work Hours`*
                                      Wages_Employment$`Median Hourly Wage`

tail(Wages_Employment)

Wages_Employment$Cumulative_Wages <- Wages_Employment$Cumulative_Wages/1000000000

Wages_Employment_Change <- Return.calculate(Wages_Employment$Cumulative_Wages)
Wages_Employment_Change_12mo <- ROC(Wages_Employment$Cumulative_Wages, n = 12)

tail(Wages_Employment_Change_12mo)

autoplot(Wages_Employment_Change_12mo)

Chart_Total_Wages <- autoplot(Wages_Employment$Cumulative_Wages) + theme_joey() +
theme(legend.position = "right", 
      plot.title = element_text(size = 12, face = "bold"),
      legend.title=element_text(size=8), 
      legend.text=element_text(size=7))  +
  labs(title ="Total Wages - Workers x Hours x Wage (Weekly; Nonsupervisory employees)", 
       x = "Date", y = "Total Earnings (Billions)", caption = "St. Louis Fed")

Chart_Change_Total_Wages <- autoplot(Wages_Employment_Change_12mo) + theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="YoY Total Wages Change", 
       x = "Date", y = "Total Earnings", caption = "St. Louis Fed")

#combine all 2 graphs
grid.arrange(
  Chart_Total_Wages, Chart_Change_Total_Wages,
  nrow = 2, ncol = 1,
  top = "Total US Wages and Changes Year over Year"
)

