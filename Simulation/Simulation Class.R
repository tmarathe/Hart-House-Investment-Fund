setwd("G:/Dropbox/HHIF Quant/Simulation")
source("Strategy Class.R")
library(stockPortfolio)
library(TTR)



setClass("Simulation", 
         representation(simulationlength="numeric",
                        currtime="numeric",
                        numstocks="numeric",
                        prices="matrix",
                        strategy="Strategy",
                        account="numeric",
                        portfoliovalue="numeric",
                        position="matrix"),
         prototype(simulationlength=100,
                   currtime=0,
                   numstocks=5,
                   prices=matrix(0, nrow=100, ncol=5),
                   strategy=new("Strategy"),
                   account=numeric(100),
                   portfoliovalue=numeric(100),
                   position=matrix(0, nrow=100, ncol=5)))

setMethod("show", "Simulation",
          function(object){
            cat("At current time,", object@currtime, ",the portfolio is as follows: \n")
            cat("Portfolio:\n")
            port <- rbind(object@position[object@currtime,],
                          object@prices[object@currtime,])
            rownames(port) <- c("Position", "Price")
            print(port)
            cat("Portfolio Value: ", object@portfoliovalue[object@currtime], "\n")
            cat("Account Value: ", object@account[object@currtime], "\n")
            cat("Strategy:", show(object@strategy), "\n")
          })
setMethod("plot", "Simulation", 
          function(object, x=1:(object@simulationlength), y=object@portfoliovalue, 
                   scale=F, ylim=NULL)
          {
            if(scale==T){
              plot(x=1:(object@simulationlength), y=(object@portfoliovalue)/object@portfoliovalue[1], 
                   xlab="time", ylab="scaled portfolio value", type="l", ylim=ylim)
            }
            else if(scale==F){
              plot(x=1:(object@simulationlength), y=object@portfoliovalue, 
                   xlab="time", ylab="portfolio value", type="l", ylim=ylim)
            }
          })

Simulation.Start <- 
  function(Sim, initposition, initaccount=0)
  {
    if(Sim@currtime!=0) return("ERROR: Simulation already Started")
    Sim@account[1] <- initaccount
    Sim@position[1,] <- initposition
    Sim@portfoliovalue[1] <- as.numeric(rbind(Sim@position[1,])%*%cbind(Sim@prices[1,]))
    Sim@currtime <- 1
    return(Sim)
  }

Simulation.oneForward <- 
  function(Sim)
  {
    PositionChange <- Sim@strategy@strategy(Sim@strategy, Sim@prices[Sim@currtime,], 
                                            Sim@position[Sim@currtime,], Sim@accountvalue[Sim@currtime])
    Sim@account[Sim@currtime+1] <- Sim@account[Sim@currtime] - as.numeric(rbind(PositionChange)%*%cbind(Sim@prices[Sim@currtime,]))
    Sim@position[Sim@currtime+1,] <- Sim@position[Sim@currtime,] + PositionChange
    Sim@portfoliovalue[Sim@currtime+1] <- as.numeric(rbind(Sim@position[Sim@currtime+1,])%*%cbind(Sim@prices[Sim@currtime+1,]))
    Sim@currtime <- Sim@currtime+1
    return(Sim)
  }

Simulation.runAll <- 
  function(Sim)
  {
    for(i in 1:(Sim@simulationlength-Sim@currtime))
    {
      Sim <- Simulation.oneForward(Sim)
    }
    return(Sim)
  }


simprice1 <- function(curprice, histmean, histcov, numiter){
  if(length(histmean)!=length(curprice)) return("ERROR: Historical mean and current price are of different length")
  nstock <- length(histmean)
  prices <- matrix(0, nrow=numiter, ncol=nstock)
  cd <- chol(histcov)
  prices[1,] <- curprice
  for(i in 2:numiter){
    prices[i,] <- prices[i-1,]*exp((t(cd)%*%cbind(rnorm(n,0,1))+histmean))
  }
  return(prices)
}

simprice <- function(tickers, curprice, histmean, histcov, simlength, shockorigin, shockdirection=c(-1, +1), shocksize, shocktime){
  if((length(histmean)!=length(curprice)) || (length(tickers)!=length(curprice))) return("ERROR: Different lengths")
  nstock <- length(histmean)
  prices <- matrix(0, nrow=simlength, ncol=nstock)
  cd <- chol(histcov)
  originpos <- which(tickers==shockorigin)
  prices[1,] <- curprice
  shock <- shocksize*histcov[originpos,]/histcov[originpos,originpos]
  for(i in 2:simlength){
    dayreturn <- t(cd)%*%cbind(rnorm(n,0,1))+histmean+(if(i != shocktime) 0 else shock)
    prices[i,] <- prices[i-1,]*exp(dayreturn)
  }
  return(prices)
}