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
Coincident_Indicator_Symbols <- c("YALE/US_CONF_INDEX_VAL_INDIV",
                                  "YALE/US_CONF_INDEX_VAL_INST",
                                  "YALE/US_CONF_INDEX_CRASH_INDIV",
                               "YALE/US_CONF_INDEX_CRASH_INST",
                               "YALE/US_CONF_INDEX_BUY_INDIV",
                               "YALE/US_CONF_INDEX_BUY_INST",
                               "YALE/US_CONF_INDEX_1YR_INDIV",
                               "YALE/US_CONF_INDEX_1YR_INST")
                              

#download the symbols
Yale_Confidence_Surveys <- Quandl(Coincident_Indicator_Symbols, type = "xts",
                start_date="1900-01-01", end_date= Sys.Date(),
                collapse = "monthly")
Yale_Confidence_Surveys <- Yale_Confidence_Surveys[,c(1,3,5,7,9,11,13,15)]
head(Yale_Confidence_Surveys)
#rename the column names to be intuitive that will fit on the graphs
colnames(Yale_Confidence_Surveys) <- 
  c("Individual",
    "Institutional",
    "Individual",
    "Institutional",
    "Individual",
    "Institutional",
    "Individual",
    "Institutional"
   )

#choose a start date for the graphs
START_DATE <- "2006/"

#create each graph
G1 <- autoplot(Yale_Confidence_Surveys[,c(1,2)][START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black")) +
  labs(title ="Valuation Index", subtitle = "Percent of the Population Who Think The Market is not Too High", 
       x = "Date", y = "Percent")

G2 <- autoplot(Yale_Confidence_Surveys[,c(3,4)][START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="Crash Confidence Index", subtitle = "Percent Confident There Will Be No Crash in Next 6 Months", 
       x = "Date", y = "Percent")

G3 <- autoplot(Yale_Confidence_Surveys[,c(5,6)][START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7), 
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black")) +
  labs(title ="Buy on the Dips Confidence Index", 
       subtitle = "Percent of population expecting a rebound the next day should the market ever drop 3% in one day",
       x = "Date", y = "Percent")

G4 <- autoplot(Yale_Confidence_Surveys[,c(7,8)][START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black")) +
  labs(title ="One Year Index", 
       subtitle = "The percent of the population expecting an increase in the Dow in the coming year",
       x = "Date", y = "Percent", caption = "Source: Yale School of Management")

#combine all 4 graphs
grid.arrange(
 G1, G2, G3, G4,
  nrow = 2, ncol = 2,
  top = "Yale Confidence Surveys"
)


