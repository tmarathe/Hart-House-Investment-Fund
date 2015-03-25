setwd("G:/Dropbox/HHIF Quant/Simulation")
source("Simulation Class.R")
source("Strategy Class.R")
library(stockPortfolio)
library(Matrix)

holdings <- read.csv("Feb28_holdings.csv", stringsAsFactors=F)
holdings$Ticker[4] <- "CELG"
stresstickers <- c("BNO")

SP500 <- read.csv("SP500.csv", stringsAsFactors=F)
sptickers <- SP500$Ticker.symbol
sps <- sample(sptickers, 100)
othertickers <- c("^FVX", sps)
nsp <- length(sps)
#---- Data Gathering -------
tickers <- unique(c(stresstickers,unlist(holdings$Ticker), sps))
n <- length(tickers)

stockdata <- getReturns(tickers, freq="day", get="overlapOnly", start="2000-01-01")

logreturns <- log(1+stockdata$R)
histcovs <- cov(logreturns)
histmeans <- colMeans(logreturns)

curprice <- numeric(n)
for(i in 1:n){
  curprice[i] <- stockdata[[6]][[i]][1,7]
}

currentholdings <- as.numeric(gsub(",", "", holdings$Position))



#---- Looping simulated cases ------
set.seed(1)
niter <- 1000 #Num of cases
corrs <- numeric(niter)
corrs2 <- numeric(niter)
oilbeta <- numeric(niter)
simlength <- 60
oilshock <- numeric(niter)
portshock <- numeric(niter)
sharpe <- numeric(niter)
avgport <- numeric(simlength)
avgoil <- numeric(simlength)
# plot(1:simlength,rep(0, simlength),col="white", ylim=c(0.7, 1.3))
for(i in 1:niter){
  futureprices <- simprice(tickers, curprice, histmeans, histcovs, simlength=simlength, 
                           shockorigin="BNO", shockdirection=-1, shocksize=-0.2, shocktime=30)
#   hcol <- rainbow(n)
#   plot(x=c(1:60), col="white", ylim=c(0.5, 1.5))
#   par(mfrow=c(1,1))
#   for(j in 1:n){
#     lines(x=c(1:60), y=futureprices[,i]/futureprices[1,i], col=hcol[i], type="l")
#   }
  donothing <- new("Strategy", numstocks=n, position=numeric(n))
  donothing
  sim1 <- new("Simulation", simulationlength=simlength, numstocks=n, prices=futureprices, 
              strategy=donothing, account=numeric(simlength), portfoliovalue=numeric(simlength),
              position=matrix(0, nrow=simlength, ncol=n))
  sim1 <- Simulation.Start(Sim=sim1, 
                           initposition=c(rep(0, length(stresstickers)), 
                                          currentholdings, 
                                          rep(0, n-length(stresstickers)-length(currentholdings))))
  sim1 <- Simulation.runAll(sim1)
  futureoilreturn <- log(futureprices[2:(simlength),1]/futureprices[1:(simlength-1),1])
  portfolioreturn <- log(sim1@portfoliovalue[2:(simlength)]/sim1@portfoliovalue[1:(simlength-1)])

  corrs[i] <- cor(futureoilreturn, portfolioreturn)
  corrs2[i] <- cor(futureprices[,1], sim1@portfoliovalue)
  oilbeta[i] <- cov(futureoilreturn, portfolioreturn)/var(futureoilreturn)
  oilshock[i] <- futureoilreturn[29]
  portshock[i] <- portfolioreturn[29]
  sharpe[i] <- mean(portfolioreturn)/sqrt(var(portfolioreturn))
  avgport <- 1/i*sim1@portfoliovalue + (i-1)/i*avgport
  avgoil <- 1/i*futureprices[,1] + (i-1)/i*avgoil
#   plot(sim1, scale=T, ylim=c(0.7, 1.3))
#   lines(futureprices[,17]/futureprices[1,17], col="red")
#   lines(sim1@portfoliovalue/sim1@portfoliovalue[1], col="blue")
#   legend(x=1, y=1.2, legend=c("portfolio", "BNO"), lty=1, col=c("black", "red"))
}
mean(corrs)
mean(corrs2)
mean(oilbeta)
mean(oilshock)
mean(portshock)
mean(sharpe)
plot(oilshock, portshock)
plot(avgport/avgport[1], type="l", ylim=c(0.7, 1.1))
lines(avgoil/avgoil[1], col="red")
