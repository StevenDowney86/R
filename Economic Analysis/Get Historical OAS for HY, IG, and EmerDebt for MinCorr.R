library(Quandl)
library(quantmod)
library(tidyverse)
library(dygraphs)
library(gridExtra)
library(ggthemes)
library(grid)

#set dates
start <- as.Date("1900-01-01")
end <- Sys.Date()
#download data
HighYieldOAS <- Quandl("ML/HYOAS", api_key="MFiHkkmpYSxDhfZ1ygrU", type = "xts")
ACORP_OAS <- getSymbols("BAMLC0A3CA", src = "FRED", from = start, to = end, auto.assign = FALSE)
Acorp_Yield <- getSymbols("BAMLC0A3CAEY", src = "FRED", from = start, to = end, auto.assign = FALSE)
EmergingM_OAS <- getSymbols("BAMLEMPUPUBSLCRPIUSOAS", src = "FRED", from = start, to = end, auto.assign = FALSE)

#find the most recent OAS
tail(HighYieldOAS, n = 30)
tail(ACORP_OAS, n = 30)
tail(EmergingM_OAS, n = 30)
###percentile for US High Yield OAS daily
colnames(HighYieldOAS) <- "High Yield OAS"
HighYieldOAS_percentile <- ecdf(HighYieldOAS)
HYG_percentile_rank <- round(HighYieldOAS_percentile(last(HighYieldOAS)),3)
HYG_percentile_rank
last(HighYieldOAS)

###percentile for Corp A OAS daily
colnames(ACORP_OAS) <- "Corp A OAS"
ACORP_OAS_percentile <- ecdf(ACORP_OAS)
ACorp_Percntile_rank <- round(ACORP_OAS_percentile(
  last(ACORP_OAS)),3)
ACorp_Percntile_rank
last(ACORP_OAS)
### not working right now
colnames(EmergingM_OAS) <- "Emerging Market Debt OAS"
EmergingM_OAS_percentile <- ecdf(EmergingM_OAS)
EmergCorp_Percntile_rank <- round(EmergingM_OAS_percentile(
  last(EmergingM_OAS)),3)
EmergCorp_Percntile_rank
EmergingM_OAS_percentile(last(EmergingM_OAS))
last(EmergingM_OAS)

head(EmergingM_OAS)
OAS_Merged <- merge(ACORP_OAS, EmergingM_OAS, HighYieldOAS)


#add a dygraph with range selector
dygraph(OAS_Merged) %>% dyRangeSelector()

HighYieldOAS
last(HighYieldOAS)
HighYieldOAS_percentile()

columnames <- c("Investment Grade","High Yield")

Rank <- cbind(ACorp_Percntile_rank,HYG_percentile_rank)
Rank
rownames(Rank) <- "Historical OAS Percentile"
colnames(Rank) <- columnames

Current_OAS <- (merge(ACORP_OAS,HighYieldOAS))
Current_OAS_last <- as.data.frame(last(Current_OAS))

rownames(Current_OAS_last) <- "Current OAS"


colnames(Current_OAS_last) <- columnames

Combo <- rbind(Current_OAS_last,Rank)                    

#create table with data
grid.arrange(
  tableGrob(
    Combo, rows = rownames(Combo), cols = colnames(Combo),
          theme = ttheme_default()))
