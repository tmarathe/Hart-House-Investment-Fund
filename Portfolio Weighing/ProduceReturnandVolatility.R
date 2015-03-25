library(stockPortfolio)
library(quadprog)
library(cubature)
source("InferenceModel.R", echo=F, print.eval=F, prompt.echo=F, verbose=F)

stocks <- read.csv("StockTargetPrices.csv", header=T, stringsAsFactors=F)
tickers <- stocks$Ticker; targetPrices <- stocks$Target.Price; n <- length(tickers)
estimates <- matrix(0, nrow=n, ncol=6)
colnames(estimates) <- c("Ticker", "Current Price", "Target Price", "Expected Return", "Estimated Volatility", "Estimated Beta")
for(i in 1:n){
  estimates[i,] <- c(estimate(tickers[i], targetPrices[i]), estBeta(tickers[i]))
}
estimatedData <- data.frame(estimates)
write.csv(estimatedData, "EstimatedStockData.csv")


ETFs <- read.csv("Sector ETFs.csv", header=T, stringsAsFactors=F)
ETFTickers <- ETFs$Ticker; ETFSectors <- ETFs$Sector; ETFn <- length(ETFTickers)
ETFestimates <- matrix(0, nrow=ETFn, ncol=5)
colnames(ETFestimates) <- c("Sector", "Ticker", "Expected Return", "Estimated Volatility", "Estimated Beta")
ETFR <- colMeans(log(getReturns(ETFTickers, freq="month", get="overlapOnly", start="2004-1-1")$R+1))*12
for(i in 1:ETFn){
  ETFestimates[i,] <- c(ETFSectors[i], ETFTickers[i], ETFR[i], VIXreg(ETFTickers[i]), estBeta(ETFTickers[i]))
}
ETFdata <- data.frame(ETFestimates)
write.csv(ETFdata, "EstimatedETFData.csv")