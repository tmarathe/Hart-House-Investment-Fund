library(cubature)                                 #use install.packages("cubature") if you don't already have it

#Likelihood function
likelihood <- function(par, x, log=FALSE){
  mu <- par[1]; nu <- par[2]
  loglikelihood <- sum(dnorm(x, mean=mu, sd=sqrt(nu), log=T))
  if(log==TRUE){
    loglikelihood
  }
  else{
    exp(loglikelihood)
  }
}

#Prior information
priorinfo <- function(expectedPrice, currentPrice){
  alphahat <- log(expectedPrice/currentPrice)
  sigmaalpha <- (-alphahat/qnorm(0.4))
  report <- list("alphahat"=alphahat, "sigmaalpha"=sigmaalpha)
  return(report)
}

#Regression on VIX
VIXreg <- function(ticker){
  SVIX <- getReturns(c(ticker, "^VIX"), freq="daily",get="overlapOnly", start="2004-01-01")
  stockR <- log(1+SVIX$R[,1])
  n <- length(stockR)
  VIX <- SVIX$full$"^VIX"
  theDates <- as.Date(VIX$Date, "%Y-%m-%d")
  theYear <- format(theDates, "%Y")
  theMonth <- format(theDates, "%m")
  VIXAdj.Close <- VIX$Adj.Close
  #dailyVIX <- data.frame(cbind("Year"=VIXyear, "Month"=VIXmonth, "Day"=VIXday, "Adj.Close"=VIXAdj.Close))
  #dailyVIX$Adj.Close <- as.numeric(levels(dailyVIX$Adj.Close))[dailyVIX$Adj.Close]
  sigsq <- c(); VIXx <- c(); k <- 1
  for(i in 1:(length(stockR)-1)){
    cond1 <- theMonth[i]==theMonth[i+1]
    cond2 <- theYear[i]==theYear[i+1]
    if((cond1 & cond2)==FALSE){
      k <- c(k, i)
    }
  }
  for(j in 2:(length(k)-1)){
    sigsq <- c(sigsq, var(stockR[k[j-1]:k[j]]))
    VIXx <- c(VIXx, mean(VIXAdj.Close[k[j]:(k[j+1])]))
  }
  sig <- sqrt(sigsq)
  sigVIXdata <- data.frame(cbind(sig, VIXx))
  linmodel <- lm(sig~VIXx, data=sigVIXdata)
  coeff <- linmodel$coefficients
  sighat <- as.numeric(coeff[1]+mean(VIXAdj.Close[1:21])*coeff[2])*sqrt(365)
  sighat
}


#Obtain posterior density
#prior density pi(alpha)=dnorm(alpha, alphahat, sigmaalpha2)
piL <- function(alpha, r, alphahat, sighat, sigmaalpha){
  mu <- (alpha-sighat^2/2)/12; nu <- sighat^2/12
  dnorm(alpha, mean=alphahat, sd=sigmaalpha)*likelihood(c(mu, nu), r)
} 
coeffk <- function(piL, r, alphahat, sighat, sigmaalpha){
  (adaptIntegrate(piL, lower=-1000, upper=1000, r=r, alphahat=alphahat, sighat=sighat,sigmaalpha=sigmaalpha, tol=1e-10, maxEval=10000)$integral)^-1
}
postd <- function(alpha,r, alphahat, sighat, sigmaalpha){
  coeffk(piL, r, alphahat, sighat, sigmaalpha)*piL(alpha, r, alphahat, sighat, sigmaalpha)
}
negpostd <- function(alpha,r, alphahat, sighat, sigmaalpha){
  -postd(alpha,r, alphahat, sighat, sigmaalpha)
}

#Use posterior density to estimate
estimate <- function(ticker, targetprice){
  currentPrice <- getReturns(ticker, freq="day", get="all", Sys.Date()-10)$full[[1]]$Adj.Close[1]
  report <- priorinfo(targetprice, currentPrice)
  alphahat <- report$alphahat; sigmaalpha <- report$sigmaalpha
  sighat <- VIXreg(ticker)
  SR <- getReturns(ticker, freq="month",get="all", start="2004-01-01")
  alphaest <- nlm(negpostd, alphahat, r=log(1+SR$R), alphahat=alphahat, sighat=sighat, sigmaalpha=sigmaalpha)$estimate
  result <- c("Ticker"=ticker, "Current Price"=currentPrice, "Target Price"=targetprice, "Expected Return"=alphaest-sighat^2/2, "Estimated Volatility"=sighat)
  return(result)
}


#Beta Estimation
estBeta <- function(ticker){
  Data <- getReturns(c(ticker, "^GSPC"), freq="day", get="overlapOnly", start="2004-01-01")
  stockR <- log(1+Data$R[,1]); mktR <- log(1+Data$R[,2])
  sigim <- cov(stockR, mktR)
  sigmkt2 <- var(mktR)
  beta <- sigim/sigmkt2
  return(c("Beta"=beta))
}