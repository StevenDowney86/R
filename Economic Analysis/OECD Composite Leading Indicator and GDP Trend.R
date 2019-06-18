library(quantmod)
library(tidyverse)
library(Quandl)
library(PerformanceAnalytics)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)

#This R Script is meant to download and plot OECD Leading Economics Indicator 
#and the respective GDP Trend Indicator.

#this script will download the MinCorr and my preferred Alternative ETFs and Mutual Funds
#from alphavantage and calculate their returns YTD, and will show them in different charts.

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

GDPTrend <-Quandl("OECD/MEI_CLI_LORSGPRT_USA_M", 
                  api_key="keyhere",
                  type = "xts")

OECD_LEI <- Quandl("OECD/KEI_LOLITOAA_USA_ST_M", 
       api_key="keyhere", 
       start_date="1955-01-31")

OECD_LEI_xts <- as.xts(OECD_LEI[,2], order.by = OECD_LEI$Date)

#choose the start date for chart
START_DATE <- "2000-01-01/"

Merge_GDPTrend_OECD_LEI <- na.omit(merge(GDPTrend,OECD_LEI_xts))


autoplot(Merge_GDPTrend_OECD_LEI[START_DATE], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"))  +
  labs(title ="OECD USA LEI and GDP Trend",
       subtitle = "100 equals long term trend and deviations indicate slowing/expanding growth; LEI aims to anticipate by 4-6 months",
       x = "Date", y = "Index", caption = "Source: Quandle, OECD")


