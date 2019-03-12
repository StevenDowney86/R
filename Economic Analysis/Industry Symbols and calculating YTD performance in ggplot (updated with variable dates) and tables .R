library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(gtable)
library(grid)

#look at https://www.ftportfolios.com/retail/etf/etflist.aspx?Type=Sector
#https://www.ishares.com/us/products/etf-product-list#!type=ishares&tab=performance&view=list&fst=51670&subtab=navQuarterly
#CARZ, home builders,
#Defensive - healthcare, utilities, staples

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

MinCorr_Symbols <- c("EPP",
"IEV",
"IWB",
"EWJ",
"VWO",
"ILF",
"RWO",
"GLD",
"GSG",
"IEF",
"SHY",
"BWX",
"TIP",
"LQD",
"EMB",
"HYG",
"TLT",
"FXI",
"BKLN",
"FLOT",
"GCC")

Alternative_ETFs_MFs <- c("HDGE",
                      "TAIL",
                      "GMOM",
                      "QAI",
                      "DIVY",
                      "AMFAX",
                      "MFTNX")

#download symbols for MinCorr and Alternative ETFs & MFs
setDefaults(getSymbols.av, api.key = "YG136Y1AT346MEHI")
getSymbols(MinCorr_Symbols, src = "av", 
           output.size = "full", adjusted = TRUE)
getSymbols(Alternative_ETFs_MFs, src = "av", 
           output.size = "full", adjusted = TRUE)

#Choose the date you want to use to start the analysis
Start_Date <- "2017-12-31/"

#truncate data to start from beginning of the year
EPP <- EPP[Start_Date]
IEV <- IEV[Start_Date]
IWB <- IWB[Start_Date]
EWJ <- EWJ[Start_Date]
VWO <- VWO[Start_Date]
ILF <- ILF[Start_Date]
RWO <- RWO[Start_Date]
GLD <- GLD[Start_Date]
GSG <- GSG[Start_Date]
IEF <- IEF[Start_Date]
SHY <- SHY[Start_Date]
BWX <- BWX[Start_Date]
TIP <- TIP[Start_Date]
LQD <- LQD[Start_Date]
EMB <- EMB[Start_Date]
HYG <- HYG[Start_Date]
TLT <- TLT[Start_Date]
FXI <- FXI[Start_Date]
BKLN <- BKLN[Start_Date]
FLOT <- FLOT[Start_Date]
GCC <- GCC[Start_Date]
GMOM <- GMOM[Start_Date]
AMFAX <- AMFAX[Start_Date]
DIVY <- DIVY[Start_Date]
HDGE <- HDGE[Start_Date]
MFTNX <- MFTNX[Start_Date]
QAI <- QAI[Start_Date]
TAIL <- TAIL[Start_Date]

#calculate returns
EPP$Returns <- Return.calculate(EPP$EPP.Adjusted)
IEV$Returns <- Return.calculate(IEV$IEV.Adjusted)
IWB$Returns <- Return.calculate(IWB$IWB.Adjusted)
EWJ$Returns <- Return.calculate(EWJ$EWJ.Adjusted)
VWO$Returns <- Return.calculate(VWO$VWO.Adjusted)
ILF$Returns <- Return.calculate(ILF$ILF.Adjusted)
RWO$Returns <- Return.calculate(RWO$RWO.Adjusted)
GLD$Returns <- Return.calculate(GLD$GLD.Adjusted)
GSG$Returns <- Return.calculate(GSG$GSG.Adjusted)
IEF$Returns <- Return.calculate(IEF$IEF.Adjusted)
SHY$Returns <- Return.calculate(SHY$SHY.Adjusted)
BWX$Returns <- Return.calculate(BWX$BWX.Adjusted)
TIP$Returns <- Return.calculate(TIP$TIP.Adjusted)
LQD$Returns <- Return.calculate(LQD$LQD.Adjusted)
EMB$Returns <- Return.calculate(EMB$EMB.Adjusted)
HYG$Returns <- Return.calculate(HYG$HYG.Adjusted)
TLT$Returns <- Return.calculate(TLT$TLT.Adjusted)
FXI$Returns <- Return.calculate(FXI$FXI.Adjusted)
BKLN$Returns <- Return.calculate(BKLN$BKLN.Adjusted)
FLOT$Returns <- Return.calculate(FLOT$FLOT.Adjusted)
GCC$Returns <- Return.calculate(GCC$GCC.Adjusted)
GMOM$Returns <- Return.calculate(GMOM$GMOM.Adjusted)
AMFAX$Returns <- Return.calculate(AMFAX$AMFAX.Adjusted)
DIVY$Returns <- Return.calculate(DIVY$DIVY.Adjusted)
HDGE$Returns <- Return.calculate(HDGE$HDGE.Adjusted)
MFTNX$Returns <- Return.calculate(MFTNX$MFTNX.Adjusted)
QAI$Returns <- Return.calculate(QAI$QAI.Adjusted)
TAIL$Returns <- Return.calculate(TAIL$TAIL.Adjusted)

#Replace NA's with 0 as it is the first return point
EPP[is.na(EPP)] <- 0
IEV[is.na(IEV)] <- 0
IWB[is.na(IWB)] <- 0
EWJ[is.na(EWJ)] <- 0
VWO[is.na(VWO)] <- 0
ILF[is.na(ILF)] <- 0
RWO[is.na(RWO)] <- 0
GLD[is.na(GLD)] <- 0
GSG[is.na(GSG)] <- 0
IEF[is.na(IEF)] <- 0
SHY[is.na(SHY)] <- 0
BWX[is.na(BWX)] <- 0
TIP[is.na(TIP)] <- 0
LQD[is.na(LQD)] <- 0
EMB[is.na(EMB)] <- 0
HYG[is.na(HYG)] <- 0
TLT[is.na(TLT)] <- 0
FXI[is.na(FXI)] <- 0
BKLN[is.na(BKLN)] <- 0
FLOT[is.na(FLOT)] <- 0
GCC[is.na(GCC)] <- 0
GMOM[is.na(GMOM)] <- 0
AMFAX[is.na(AMFAX)] <- 0
DIVY[is.na(DIVY)] <- 0
HDGE[is.na(HDGE)] <- 0
MFTNX[is.na(MFTNX)] <- 0
QAI[is.na(QAI)] <- 0
TAIL[is.na(TAIL)] <- 0

#Calculate cumulative growth
EPP$EPPCumGrowth <- cumprod(1 + (EPP$Returns)) - 1
IEV$IEVCumGrowth <- cumprod(1 + (IEV$Returns)) - 1
IWB$IWBCumGrowth <- cumprod(1 + (IWB$Returns)) - 1
EWJ$EWJCumGrowth <- cumprod(1 + (EWJ$Returns)) - 1
VWO$VWOCumGrowth <- cumprod(1 + (VWO$Returns)) - 1
ILF$ILFCumGrowth <- cumprod(1 + (ILF$Returns)) - 1
RWO$RWOCumGrowth <- cumprod(1 + (RWO$Returns)) - 1
GLD$GLDCumGrowth <- cumprod(1 + (GLD$Returns)) - 1
GSG$GSGCumGrowth <- cumprod(1 + (GSG$Returns)) - 1
IEF$IEFCumGrowth <- cumprod(1 + (IEF$Returns)) - 1
SHY$SHYCumGrowth <- cumprod(1 + (SHY$Returns)) - 1
BWX$BWXCumGrowth <- cumprod(1 + (BWX$Returns)) - 1
TIP$TIPCumGrowth <- cumprod(1 + (TIP$Returns)) - 1
LQD$LQDCumGrowth <- cumprod(1 + (LQD$Returns)) - 1
EMB$EMBCumGrowth <- cumprod(1 + (EMB$Returns)) - 1
HYG$HYGCumGrowth <- cumprod(1 + (HYG$Returns)) - 1
TLT$TLTCumGrowth <- cumprod(1 + (TLT$Returns)) - 1
FXI$FXICumGrowth <- cumprod(1 + (FXI$Returns)) - 1
BKLN$BKLNCumGrowth <- cumprod(1 + (BKLN$Returns)) - 1
FLOT$FLOTCumGrowth <- cumprod(1 + (FLOT$Returns)) - 1
GCC$GCCCumGrowth <- cumprod(1 + (GCC$Returns)) - 1
GMOM$GMOMCumGrowth <- cumprod(1 + (GMOM$Returns)) - 1
AMFAX$AMFAXCumGrowth <- cumprod(1 + (AMFAX$Returns)) - 1
DIVY$DIVYCumGrowth <- cumprod(1 + (DIVY$Returns)) - 1
HDGE$HDGECumGrowth <- cumprod(1 + (HDGE$Returns)) - 1
MFTNX$MFTNXCumGrowth <- cumprod(1 + (MFTNX$Returns)) - 1
QAI$QAICumGrowth <- cumprod(1 + (QAI$Returns)) - 1
TAIL$TAILCumGrowth <- cumprod(1 + (TAIL$Returns)) - 1

#combine all of the symbols from Mincorr
COMBO <- merge(EPP,
               EWJ,
               FXI,
               GLD,
               GSG,
               HYG,
               IEF,
               IEV,
               ILF,
               IWB,
               LQD,
               RWO,
               TIP,
               TLT,
               VWO,
               EMB,
               BWX,
               SHY,
               BKLN,
               FLOT,
               GCC)
#combine all of my preferred alternative strategy ETFs and MFs
COMBO_2 <- merge(GMOM,
                 AMFAX,
                 DIVY,
                 HDGE,
                 MFTNX,
                 QAI,
                 TAIL)

#Merge only the cumulative growth columns to use in ggplot
Combo_CumGrowth <- COMBO[,c(seq(8,168,8))]
COMBO_2_CumGrowth <- COMBO_2[,c(seq(8,56,8))]

#rename the columns to be more intuitive with ticker symbols
colnames(Combo_CumGrowth) <- c("Pacific ex-Japan Equity (EPP)",
                                "Japan Equity (EWJ)",
                                "China Equity (FXI)",
                                "Gold (GLD)",
                                "Commodities (GSG)",
                                "USA High Yield (HYG)",
                                "USA 7-10 Year Treasuries (IEF)",
                                "Developed Europe Equity (IEV)",
                                "Latin America Equity (ILF)",
                                "USA Large Cap Equity (IWB)",
                                "USA Inv Grade Bonds (LQD)",
                                "Global Real Estate (RWO)",
                                "USA TIPS (TIP)",
                                "USA 20+ Years Treasuries (TLT)",
                                "Emerging Market Equity (VWO)",
                                "Emerging Market Bonds (EMB)",
                                "Int'l Treasury Bonds (BWX)",
                                "USA 1-3 Year Treasuries (SHY)",
                               "High Yield Floating Sen. Loans (BKLN)",
                               "Investment Grade Floating Rate (FLOT)",
                               "Commodities Equal Weight (GCC)")
colnames(COMBO_2_CumGrowth) <- c("Cambria Global Momentum (GMOM)",
                                 "Natixis Managed Futures (AMFAX)",
                                 "Realtyshares DIVY (DIVY)",
                                 "Ranger Dedicated Short (HDGE)",
                                 "Arrow_DUNN Trend (MFTNX)",
                                 "IQ Hedge Multi-Strategy (QAI)",
                                 "Cambria Tail Risk (TAIL)")

#creat ggplot in autoplot
Fixed_Income <-autoplot(Combo_CumGrowth[,c(6,7,11,13,14,16,17,18,19,20)], facets = NULL) +
theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7)) +
  labs(title ="Fixed Income", 
       x = "Date", y = "Percent Growth")

Equities <- autoplot(Combo_CumGrowth[,c(1,2,3,8,9,10,15)], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Equities", 
       x = "Date", y = "Percent Growth")

Alternatives <- autoplot(Combo_CumGrowth[,c(4,5,12,21)], facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Alternatives",
       x = "Date", y = "Percent Growth") 


Alternative_ETFs_MFs_Graph <- autoplot(COMBO_2_CumGrowth, facets = NULL) +
  theme_joey() +
  theme(legend.position = "right", 
        plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=7))  +
  labs(title ="Alternative Strategies",
       x = "Date", y = "Percent Growth", caption = "Source: Alphavantage")

grid.arrange(
  Equities,
  Alternatives,
  Fixed_Income,
  Alternative_ETFs_MFs_Graph,
  nrow = 2, ncol = 2,
  top = "Year to Date Total Return Performance of Major Asset Classes"
  )

#Risk Stats for ETFS

head(COMBO)
