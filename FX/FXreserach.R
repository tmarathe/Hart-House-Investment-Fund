library(stockPortfolio)
library(quantmod)
library(TTR)
USDCAD1<-getReturns('CAD=X', freq = "day", get = "overlapOnly", start="2014-01-01", end = NULL)
USDCAD1$full$CAD$Close
#yahoofinance is too inaccurate 

#data from oanda
#midpoint data

setwd("C:/Users/Kevin Hu/Desktop/Dropbox/HHIF Quant/FX")
USDEUR<-rev(read.csv("USDEUR.csv", header=T,stringsAsFactors=F)$USDEUR)#data is from most recent
AUDJPY<-rev(read.csv("AUDJPY.csv", header=T,stringsAsFactors=F)$AUDJPY)
USDCAD<-rev(read.csv("USDCAD.csv", header=T,stringsAsFactors=F)$USDCAD)

#calculating volatility (try exponentially weighted)
period=2
USDEUR_sd <- sd(returns(USDEUR)[period:length(USDEUR)])*252^0.5
AUDJPY_sd <- sd(returns(AUDJPY)[period:length(AUDJPY)])*252^0.5
USDCAD_sd <- sd(returns(USDCAD)[period:length(USDCAD)])*252^0.5

#using exponential moving average 
lookback=120 #6months
plot(EMA(returns(USDEUR), n =lookback),main="USDEUR")
plot(EMA(returns(AUDJPY), n =lookback),main="AUDJPY")
abline(0,0)

#return on momentum trading 