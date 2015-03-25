setwd("G:/Dropbox/HHIF Quant/Simulation")
source("G:/Dropbox/HHIF Quant/Portfolio Weighing/InferenceModel.R", echo=F, print.eval=F, prompt.echo=F, verbose=F)
source("Simulation Class.R", echo=F, print.eval=F, prompt.echo=F, verbose=F)
source("Strategy Class.R", echo=F, print.eval=F, prompt.echo=F, verbose=F)
library(stockPortfolio)
library(corrplot)
library(Matrix)

SP500 <- read.csv("SP500.csv", stringsAsFactors=F)

holdings <- read.csv("Feb28_holdings.csv", stringsAsFactors=F)
holdings$Ticker[4] <- "CELG"
tickers <- c(unlist(holdings$Ticker), "BNO")
n <- length(tickers)

stockdata <- getReturns(tickers, freq="day", get="overlapOnly", start="2000-01-01")
head(stockdata$R)
#corrplot(cor(stockdata$R))
logreturns <- log(1+stockdata$R)
histcovs <- cov(logreturns)
histmeans <- colMeans(logreturns)

curprice <- histmean <- numeric(n)
for(i in 1:n){
  histmean[i] <- mean(stockdata[[6]][[i]][,7])
  curprice[i] <- stockdata[[6]][[i]][1,7]
}

currentholdings <- as.numeric(gsub(",", "", holdings$Position))

futureprices <- simprice1(curprice, histmeans, histcovs, numiter=60)
hcol <- rainbow(n)
plot(x=c(1:60), col="white", ylim=c(min(futureprices), max(futureprices)))
par(mfrow=c(1,1))
for(i in 1:n){
  lines(x=c(1:60), y=futureprices[,i], col=hcol[i], type="l")
}


#test simulation
futureprices <- simprice1(curprice, histmeans, histcovs, numiter=60)

donothing <- new("Strategy", numstocks=n, position=numeric(n))
donothing
sim1 <- new("Simulation", simulationlength=60, numstocks=n, prices=futureprices, 
            strategy=donothing, account=numeric(60), portfoliovalue=numeric(60),
            position=matrix(0, nrow=60, ncol=n))
sim1 <- Simulation.Start(Sim=sim1, initposition=c(currentholdings, 0))
sim1 <- Simulation.runAll(sim1)
sim1
plot(sim1, scale=T, ylim=c(0.8, 1.2))
lines(futureprices[,17]/futureprices[1,17], col="red")
legend(x=1, y=1.2, legend=c("portfolio", "BNO"), lty=1, col=c("black", "red"))
