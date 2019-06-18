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
Quandl.api_key("keyhere")

#set the indices to download
ISM_Manufacturing_Indices <- c("ISM/MAN_PMI",
                               "ISM/MAN_NEWORDERS",
                               "ISM/MAN_PROD",
                               "ISM/MAN_EMPL",
                               "ISM/MAN_DELIV",
                               "ISM/MAN_INVENT",
                               "ISM/MAN_PRICES")

#download the symbols
ISM_Manufacturing_Indices_data <- Quandl(ISM_Manufacturing_Indices, type = "xts",
                                      start_date="1900-01-01", end_date= Sys.Date(),
                                      collapse = "monthly")
ISM_Manufacturing_Indices_data <- ISM_Manufacturing_Indices_data[,c(1,6,11,16,21,26,31)]


#names the data
colnames(ISM_Manufacturing_Indices_data) <- c("PMI Index",
  "New Orders Index",
  "Production Index",
  "Manufacturing Employment Index",
  "Deliveries Index",
  "Inventories Index",
  "Prices Index")

#create 12 month SMA of PMI Index
PMI_Index <- na.omit(ISM_Manufacturing_Indices_data$`PMI Index`)
PMI_Index$SMA12 <- SMA(PMI_Index, n = 12)
tail(PMI_Index)

colnames(PMI_Index) <- c("PMI Index",
                     "12-Month Moving Average PMI Index")


#choose a start date for the graphs
START_DATE <- "2007/"

#create charts
ISM1 <- autoplot(merge(ISM_Manufacturing_Indices_data[,1], PMI_Index[,2])[START_DATE], 
                facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="PMI Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM2 <- autoplot(merge(ISM_Manufacturing_Indices_data[,2])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="New Orders Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM3 <- autoplot(merge(ISM_Manufacturing_Indices_data[,3])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Production Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM4 <- autoplot(merge(ISM_Manufacturing_Indices_data[,4])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Manufacturing Employment Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM5 <- autoplot(merge(ISM_Manufacturing_Indices_data[,5])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Deliveries Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM6 <- autoplot(merge(ISM_Manufacturing_Indices_data[,6])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Inventories Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")

ISM7 <- autoplot(merge(ISM_Manufacturing_Indices_data[,7])[START_DATE], 
                 facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 10, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Prices Index", 
       x = "Date", y = "Index") +
  theme(legend.position="none")


#combine all 7 graphs
ISM1

grid.arrange(
  ISM2, ISM3, ISM4, ISM5, ISM6, ISM7,
  nrow = 3, ncol = 2,
  top = "PMI Index Components")

