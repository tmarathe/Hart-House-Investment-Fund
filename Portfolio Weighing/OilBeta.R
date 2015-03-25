library(stockPortfolio)
library(quantmod)
setwd("G:/Dropbox/HHIF Quant/Portfolio Weighing")

Current.Portfolio <- read.csv("Current Portfolio.csv", header=T, stringsAsFactors=F)
Tickers <- Current.Portfolio$Ticker; Positions <- Current.Portfolio$Position
Prices <- getQuote(Tickers)[,2]
CADUSD <- getQuote("CAD/USD")[,2]/100
USDPrices <- Prices
USDPrices[Current.Portfolio$Currency=="CAD"] <- Prices[Current.Portfolio$Currency=="CAD"]*CADUSD
MarketValues <- Prices*as.numeric(gsub(",","",Positions))
Weights <- MarketValues/sum(MarketValues)

oil <- c("WTI", "BNO")
  
estBeta <- function(tickers, oilticker, weights){
  n <- length(tickers)
  Data <- getReturns(c(tickers, oilticker), freq="day", get="overlapOnly", start="2004-01-01")
  stockR <- log(1+Data$R[,1:n]); oilR <- log(1+Data$R[,n+1])
  sigim <- 0
  sigim <- sum(cov(stockR, oilR)*weights)
  sigoil2 <- var(oilR)
  beta <- sigim/sigoil2
  return(c("Beta"=beta))
}

betas <- numeric(2)

for(i in 1:2){
  betas[i] <- estBeta(Tickers, oil[i], Weights)
}

betas