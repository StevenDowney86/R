library(Quandl)
library(quantmod)
library(tidyverse)
library(dygraphs)
library(gridExtra)
library(ggthemes)
library(grid)
library(PerformanceAnalytics)

#This Data is to access information that will reveal how open or closed
#credit is to corporates and individuals

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

#set dates for data collection
start <- as.Date("1900-01-01")
end <- Sys.Date()

#download data
#get OAS
AAA_CORP_OAS <- getSymbols("BAMLC0A1CAAA", src = "FRED", from = start, to = end, auto.assign = FALSE)
A_CORP_OAS <- getSymbols("BAMLC0A3CA", src = "FRED", from = start, to = end, auto.assign = FALSE)
BB_CORP_OAS <- getSymbols("BAMLH0A1HYBB", src = "FRED", from = start, to = end, auto.assign = FALSE)
HighYieldOAS <- Quandl("ML/HYOAS", api_key="MFiHkkmpYSxDhfZ1ygrU", type = "xts")
CCC_CORP_OAS <- getSymbols("BAMLH0A3HYC", src = "FRED", from = start, to = end, auto.assign = FALSE)

#get yields
AAA_CORP_Yield <- getSymbols("BAMLC0A1CAAAEY", src = "FRED", from = start, to = end, auto.assign = FALSE)
A_CORP_Yield <- getSymbols("BAMLC0A3CAEY", src = "FRED", from = start, to = end, auto.assign = FALSE)
BB_CORP_Yield <- getSymbols("BAMLH0A1HYBBEY", src = "FRED", from = start, to = end, auto.assign = FALSE)
HighYield_Yield <- getSymbols("BAMLH0A0HYM2EY", src = "FRED", from = start, to = end, auto.assign = FALSE)
CCC_CORP_Yield <- getSymbols("BAMLH0A3HYCEY", src = "FRED", from = start, to = end, auto.assign = FALSE)

#getTedSpread
Ted_Spread <- getSymbols("TEDRATE", src = "FRED", from = start, to = end, auto.assign = FALSE)

#Data from Senior Bankers Survey
Net_Percent_Banks_Tightening_towards_MidtoLarge_Companies <- 
  getSymbols("DRTSCILM", src = "FRED", from = start, to = end, auto.assign = FALSE)
Net_Percent_Banks_Tightening_towards_Small_Companies <-
  getSymbols("DRTSCIS", src = "FRED", from = start, to = end, auto.assign = FALSE)
Net_Percent_Banks_Tightening_Standard_Consumer_Credit_Cards <-
  getSymbols("DRTSCLCC", src = "FRED", from = start, to = end, auto.assign = FALSE)

#rename the columns
colnames(Net_Percent_Banks_Tightening_towards_MidtoLarge_Companies) <-
  "Credit to Mid to Large Firms"
colnames(Net_Percent_Banks_Tightening_towards_Small_Companies) <-
  "Credit Credit to Small Firms"
colnames(Net_Percent_Banks_Tightening_Standard_Consumer_Credit_Cards) <-
  "Credit to Consumer Credit Cards"


colnames(AAA_CORP_Yield) <- "AAA Corp Yield"
colnames(A_CORP_Yield) <- "A Corp Yield"
colnames(BB_CORP_Yield) <- "BB Corp Yield"
colnames(HighYield_Yield) <- "High Yield Yield"
colnames(CCC_CORP_Yield) <- "CCC Corp Yield"


colnames(AAA_CORP_OAS) <- "AAA Corp OAS"
colnames(A_CORP_OAS) <- "A Corp OAS"
colnames(BB_CORP_OAS) <- "BB Corp OAS"
colnames(HighYieldOAS) <- "High Yield OAS"
colnames(CCC_CORP_OAS) <- "CCC Corp OAS"

colnames(Ted_Spread) <- "TED Spread"

HighYield_AAAspread <- HighYieldOAS - AAA_CORP_OAS
colnames(HighYield_AAAspread) <- "HighYield.Aaa.Spread"

#Dates for Graphs
Graph_Dates <- "2007/"

#merge the data
Senior_Loan_Officer_Survey <- merge(Net_Percent_Banks_Tightening_towards_MidtoLarge_Companies,
                                    Net_Percent_Banks_Tightening_towards_Small_Companies,
                                    Net_Percent_Banks_Tightening_Standard_Consumer_Credit_Cards)

OAS_Merged <- merge(AAA_CORP_OAS, 
                    A_CORP_OAS,
                    BB_CORP_OAS,
                    HighYieldOAS,
                    CCC_CORP_OAS)

Yields_Merge <- merge(AAA_CORP_Yield,
                      A_CORP_Yield,
                      BB_CORP_Yield,
                      HighYield_Yield,
                      CCC_CORP_Yield)

#creat ggplot in autoplot
Ted_Spread_Graph <-autoplot(merge(Ted_Spread,HighYield_AAAspread)[Graph_Dates], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7)) +
  labs(title ="TED Spread & High Yield minus Aaa Spread", 
       x = "Date", y = "Percent")

Fixed_Income_Yields_Graph <- autoplot(Yields_Merge[Graph_Dates], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Fixed Income Yields", 
       x = "Date", y = "Percent")

Fixed_Income_OAS_Graph <- autoplot(OAS_Merged[Graph_Dates], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Fixed Income Option Adjusted Spread",
       x = "Date", y = "OAS") 


Senior_Loan_Officer_Survey_Graph <- autoplot(Senior_Loan_Officer_Survey[Graph_Dates], facets = NULL) +
  theme_joey() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Net Percent Change with Bank Credit Standards",
       x = "Date", y = "Percent", caption = "Source: Merrily Lynch, St. Louis Fed")

grid.arrange(
  Ted_Spread_Graph,
  Fixed_Income_Yields_Graph,
  Fixed_Income_OAS_Graph,
  Senior_Loan_Officer_Survey_Graph,
  nrow = 2, ncol = 2,
  top = "Credit Conditions in the USA"
)


