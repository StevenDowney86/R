#quick and dirty vol target portfolio and levered portfolios
#haven't made it realistic with transaction costs, 
#removing lookahead bias 

#the cost of leverage is implicit in short cash weight via SHY or treasury rate

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)


#get ETFs and construct simply 2:1 levered portfolio
getSymbols(c("AGG", "VT", "SHY"))

returns <- merge(AGG[,6],
                 VT[,6],
                 SHY[,6]) %>% na.omit() %>% Return.calculate()

levered <- Return.portfolio(returns, weights = c(1,1,-1),
                            rebalance_on = "months")


##########import Fama French data and creating target vol portfolio#########

#create an xts dataframe from the Risk Free return and Market Return
vol_dataframe_FF <- merge(FF_3_Factor_and_Momentum$MKT_Plus_Rf,FF_3_Factor_and_Momentum$RF)

#create lagged return series to invest in to eliminate look ahead bias.
#We can only invest in the close following the signal day.
vol_dataframe_FF$lag_stock_returns <- lag.xts(vol_dataframe_FF$MKT_Plus_Rf, -1)
vol_dataframe_FF$lag_cash_returns <- lag.xts(vol_dataframe_FF$RF, -1)

#create blended rolling vol time frames to minimize parameter specification risk.
#We are trying to get the general signal vs. exactly right. Using 20 - 140 trading days as 
#the short term rolling window
equity_vol_20 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                                         width = 20)
equity_vol_40 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 40)
equity_vol_60 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 60)
equity_vol_80 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 80)
equity_vol_100 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 100)
equity_vol_120 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 120)
equity_vol_140 <- rollapply(vol_dataframe_FF$MKT_Plus_Rf, FUN = StdDev.annualized,
                           width = 140)

Equity_Vol_Combined <- merge(equity_vol_20,
                             equity_vol_40,
                             equity_vol_60,
                             equity_vol_80,
                             equity_vol_100,
                             equity_vol_120,
                             equity_vol_140)

#create blended equity vol measure

Equity_vol_averaged <- apply(Equity_Vol_Combined, FUN = mean, MARGIN = 1)

#convert to xts dataframe
Equity_vol_averaged_xts <- as.xts(as.data.frame(Equity_vol_averaged), dateFormat = "Date")

#merge the equity vol dataframe with the master dataframe
vol_dataframe_FF <- merge(vol_dataframe_FF,Equity_vol_averaged_xts)

#check to make sure it looks right
tail(vol_dataframe_FF,10)

#Choose a target vol level, 16% is the historical average equity vol, not 
#that you would have known that at the time but you can choose a level
vol_dataframe_FF$target_vol <- .16

#construct the equity weight as a relation to target vol
vol_dataframe_FF$equity_weight <- vol_dataframe_FF$target_vol/vol_dataframe_FF$Equity_vol_averaged

#Cash weight will be the opposite
vol_dataframe_FF$cash_weight <- 1 - vol_dataframe_FF$equity_weight

vol_dataframe_FF <- na.omit(vol_dataframe_FF)

#compute the before fee portfolio return
vol_dataframe_FF$portfolio_return <- (vol_dataframe_FF$MKT_Plus_Rf * vol_dataframe_FF$equity_weight) + (vol_dataframe_FF$RF * vol_dataframe_FF$cash_weight)

#check to make sure it looks right
tail(vol_dataframe_FF)

#chose transaction fee of 15 bps, though Andrew Lo used 5bps in his simulation
#but he estimates using futures the one way cost could be about 1 bps.
#However, transaction fees were about 2% before May day when commissions became
#unfixed, so you would be able to execute this strategy since 1926, but you can going
#forward.

Transaction_Fee <- 0.0015

#calculate the portfolio turnover to calculate the fee
Equity_weight_change <- lag.xts(vol_dataframe_FF$equity_weight, -1) - vol_dataframe_FF$equity_weight
Cash_weight_change <- lag.xts(vol_dataframe_FF$cash_weight, -1) - vol_dataframe_FF$cash_weight

vol_dataframe_FF$Equity_weight_change <- Equity_weight_change
vol_dataframe_FF$Cash_weight_change <- Cash_weight_change

Portfolio_Turnover <- abs(vol_dataframe_FF$Equity_weight_change) + abs(vol_dataframe_FF$Cash_weight_change)

vol_dataframe_FF$Portfolio_Turnover <- Portfolio_Turnover

vol_dataframe_FF$Transaction_Fees <- vol_dataframe_FF$Portfolio_Turnover * Transaction_Fee

vol_dataframe_FF$portfolio_return_after_fees <- vol_dataframe_FF$portfolio_return - vol_dataframe_FF$Transaction_Fees

#See if the actual porfolio vol came close to the target using 100 day, 252 days, 756 days (3 year)
vol_dataframe_FF$portfolio_return_vol_after_fees <- rollapply(vol_dataframe_FF$portfolio_return_after_fees, FUN = StdDev.annualized,
                                                width = 100)
vol_dataframe_FF$portfolio_return_vol_after_fees_252 <- rollapply(vol_dataframe_FF$portfolio_return_after_fees, FUN = StdDev.annualized,
                                                                  width = 252)
vol_dataframe_FF$portfolio_return_vol_after_fees_756 <- rollapply(vol_dataframe_FF$portfolio_return_after_fees, FUN = StdDev.annualized,
                                                                  width = 756)

#You can see using a longer measurement window it does a decent job at staying around the target
#volatility level
autoplot(vol_dataframe_FF[,c(6,15,16,17)]["2007/"], facets = NULL)

#create total wealth index of the portfolio
vol_dataframe_FF$PortfolioCumGrowth_after_fees <- cumprod(1 + (vol_dataframe_FF$portfolio_return_after_fees)) - 1

#See the performance stats for the strategy before fees and after compared
#to 100% static buy and hold equity

table.AnnualizedReturns(vol_dataframe_FF[,c(1,9,14)])
charts.PerformanceSummary(vol_dataframe_FF[,c(1,9,14)], legend.loc = "topleft", main = "Strategy Return vs. Benchmark")
maxDrawdown(vol_dataframe_FF[,c(1,9,14)])
table.DrawdownsRatio(vol_dataframe_FF[,c(1,9,14)])

FundReturns <- vol_dataframe_FF[,c(1,9,14)]

#testing to see at what fee level the strategy isn't superior than a buy and hold
#from a pure return perspective to vol.

PORTFOLIO_different_fees_35bps<- vol_dataframe_FF$Portfolio_Turnover * .0035
PORTFOLIO_different_fees_35bps$returns <- vol_dataframe_FF$portfolio_return - PORTFOLIO_different_fees_35bps$Portfolio_Turnover
table.AnnualizedReturns(PORTFOLIO_different_fees_35bps$returns)

PORTFOLIO_different_fees_50bps<- vol_dataframe_FF$Portfolio_Turnover * .0050
PORTFOLIO_different_fees_50bps$returns <- vol_dataframe_FF$portfolio_return - PORTFOLIO_different_fees_50bps$Portfolio_Turnover
table.AnnualizedReturns(PORTFOLIO_different_fees_50bps$returns)

PORTFOLIO_different_fees_40bps<- vol_dataframe_FF$Portfolio_Turnover * .0040
PORTFOLIO_different_fees_40bps$returns <- vol_dataframe_FF$portfolio_return - PORTFOLIO_different_fees_40bps$Portfolio_Turnover
table.AnnualizedReturns(PORTFOLIO_different_fees_40bps$returns)

#The equity level target vol of 16% becomes the same as pure equity exposure
#once the transaction costs are 40 bps of the portfolio, below that it is superior
#from a return perspective. This strategy works because the really bad returns
#happen when volatility is high and the strategy underweights equities in this instance
#it also overweighs equities when volatility is low which is when equities 
#generate the highest returns usually.

#############################################################################


#trying to create for loop of sequent of days to measure rolling vol but not
#working

seq(1:10)
List = list()
m <- as.data.frame(matrix(0, ncol = 6, nrow = 2747))
for (i in 90:95) {
  object <- rollapply(returns$VT.Adjusted, FUN = StdDev.annualized, width = i)
  m[i] <- object
}

nrow(returns)
