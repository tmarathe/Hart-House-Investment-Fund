#library(stockPortfolio) this package clashes with timeSeries which is used in fGarch, use quantmod instead
library(quantmod) #see quantmod.com
library(fGarch) 
#use to predict absolute risk,active risk(tracking error), and covariances 
#note if the ACF of the return series fails the Ljung-Box test then we need to first
  #fit an ARMA process 

#loading data 
return2lr<-function(returns) log(returns+1)

setwd("C:/Users/kevinhu/Dropbox/HHIF Quant/GARCH Modeling")
data<- read.csv("Portfolio_Values.csv", header=T,stringsAsFactors=F)
portfolio<-data$dailyreturn[2:length(data$dailyreturn)]
mclr_portfolio<- return2lr(portfolio-mean(portfolio))#mclr=mean corrected log-returns
S_P<-data$benchmark[2:length(data$benchmark)]

getSymbols('AAPL') 
weeklyReturn(AAPL["2014::"])
getSymbols('^GSPC')
weeklyReturn(GSPC["2011::"])

#GARCH(2,2) of absolute risk assuming t-distributed (4df) innovations and constant mean
abs<- garchFit(data =return2lr(dailyReturn(GSPC["2011::"])), cond.dist="std",formula = ~garch(2,2), trace=F) 
coef(abs)
predict(abs)
monthly_est_abs <- (predict(abs)$meanForecast[1]^0.5)*(20^0.5)
annual_est_abs <-(predict(abs)$meanForecast[1]^0.5)*(252^0.5)


#GARCH(1,1) of active risk assuming t-distributed innovations and constant mean
act<- garchFit(data = (portfolio-S_P), cond.dist="std") 
coef(act)
predict(act)
monthly_est_act <- predict(act)$meanForecast[1]*20^0.5
annual_est_act <-predict(act)$meanForecast[1]*252^0.5


#ynamic Conditional Correlation Mode