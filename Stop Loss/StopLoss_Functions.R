
#double check codes 
#write xls in table format (rebound percentage, loss percentage, expected loss)
# write a guide book and master functions (learn to index lists properly)

library(TTR)
library(PerformanceAnalytics) 
library(stockPortfolio)

price2lr  <- function(prices) diff(log(prices)) 
price2r <- function (prices)  diff(prices)/prices[1:(length(prices)-1)]
return2lr<-function(returns) log(returns+1)

log_returns<-function(stock)
{
  #stock must be a yahoo symbol in string format 
  return2lr(rev(getReturns(stock, freq = 'day', get = 'overlapOnly',start='2011-01-01', end = '2014-12-31')$R))
}

rolling_returns <- function(logreturns,period)
  #get the rolling return for a 'period' (in trading days) of time given logreturn series, 'logreturn'
{if (period < 1)
  stop("invalid number of period")
 rr<- rep(0, length(logreturns) - period + 1 ) # preallocation
 for (i in period:length(logreturns))
 {rr[(i- period + 1)] <- exp(sum(logreturns[(i- period + 1):i])) -1}
 return(rr)
}

check_losses<-function(logreturns,period,loss_percentage,filter)
{#check that there is loss execeeding a 'loss_percentage' for every combination in a 'period' of time  given logreturn series, 'logreturn'
  #return matrix where each column is the results for a specific period. For "filter" read convert 2
  if (period < 1)
    stop("invalid number of period")
  p=matrix(data=NA, nrow = length(logreturns), ncol=period)
  
  for (i in 1:period) #getting the rolling returns
  {
    rr<- rolling_returns(logreturns,i)
    p[,i][i:(length(rr)+i-1)]<- rr
  }
  
  logic <- (p <= -loss_percentage)#checking which returns are less
  logic_index<-which(logic %in% TRUE)#convert it so we know which index 
  
  #now we need to sort these vectors by their perspective periods, and delete the duplicates
  unique_losses<- unique(convert1(logic_index,length(logreturns),period))
  
  #delete consecutive vectors
  return(convert2(sort(unique_losses),filter))
}

convert1<-function(logic_ind,nrow,ncol)
{#sort these vectors by their perspective periods. Assuming logic_ind came from a logic matrix with 'nrow' and 'ncol'
  quotient<- logic_ind%/%(nrow+0.1)
  if(max(logic_ind/nrow)>ncol){warning("wrong logic matrix dimensions")}
  for (i in 1:length(logic_ind))
  {
    logic_ind[i]<-logic_ind[i]-quotient[i]*nrow
  }
  logic_ind
}

convert2<-function(unique_vec,consec)
{#takes a unique vector and find all the vectors that are 'consec' consecutive and only take the first of the vector
  d <- c(NA,diff(unique_vec))
  for (i in 1:length(unique_vec))
  {
    if(isTRUE(d[i]<=consec)) {unique_vec[i]=NA} #note  1=TRUE
  }
  unique_vec  
}

check_rebounds<-function(loss_index,logreturns,rebound_percentage,look_forward)
{#loss_index  is 1 number, it would only return the first time it hits 
  s = logreturns[loss_index]
  day= 1
  while (isTRUE(s<log(1+rebound_percentage)))
  {s=s+logreturns[loss_index+day]
   day=day+1}
  
  if ((day-1)<look_forward+1) 
    return(day-1)
  else 
    return(NA) #i-1 because the first day doesnt count
}

check_rebounds_all<-function(entire_index,logreturns,rebound_percentage,look_forward)
{
  days<- rep(0,length(entire_index))
  for (i in 1:length(entire_index))
  {
    days[i]<-check_rebounds(entire_index[i],logreturns,rebound_percentage,look_forward)
  }
  return(days)
}

loss_data<-function(stock,look_back,look_forward,filter,max_loss)
{#'stock' must be in strings
  #'look_back' is the look back period for losses in trading days
  #'look_forward' is the look forward for gains in trading days
  #'filter' is the filter period for consecutive losses. 5 seems to work well
  #'max_loss' is the maximum loss percentage (minimum loss is 0.01 and changes by 0.001)
  # returns : rebound rate, expected loss, loss percentages, ticker 
  lr<-log_returns(stock)
  losses<-seq(from=0.01,to=max_loss,by=0.001)
  rebound_rate<-rep(0,length(losses))

  for (i in 1:length(losses))
  {
    loss_index<- na.omit(check_losses(lr,look_back,losses[i],filter))
    rebounds<- check_rebounds_all(loss_index,lr,losses[i],look_forward)
    rebound_rate[i]<- length(na.omit(rebounds))/length(loss_index)
  }
  expected_loss=(1-rebound_rate)*losses
  output= list("ReboundRate"=rebound_rate,"LossPercentages"=losses,"ExpectedLoss"=expected_loss,"Ticker"=stock)
  return(output)
}

all_loss_data<-function(stocks,look_back,look_forward,filter,max_loss)
{
  #same as loss_data but now for multiple stocks 
  
  losses<-seq(from=0.01,to=max_loss,by=0.001)   #lossPercentages remain constant
  
  outputs=list("ReboundRates"=matrix(data=NA,nrow=length(losses),ncol=length(stocks)),
               "LossPercentages"=losses,
               "ExpectedLosses"=matrix(data=NA,nrow=length(losses),length(stocks)),
               "Tickers"=stocks)
 
  for(i in 1:length(stocks))
  {
    #storing data by matrix columns 
    lossdata<-loss_data(stocks[i],look_back,look_forward,filter,max_loss)
    outputs$ReboundRates[,i]<-lossdata$ReboundRate
    outputs$ExpectedLosses[,i]<-lossdata$ExpectedLoss
  }
  
  return(outputs)
}

max_expected_loss<-function (expected_loss,rebound_percentage,max_loss,cutoff)
{#'max_loss' should be the maximum loss set in the loss data
  #"expected_loss", is the output from the function "loss_data", specifically it is "lossdata$ExpectedLoss"
  #"cutoff" is the maximum loss percentage that the function will analyze. Suggested value 0.1
  #returns expected loss and the loss percentage
  loss_percentage<-seq(0.01,max_loss,by=0.001)
  c<-max(which(loss_percentage<cutoff))
  vec<-((1-rebound_percentage)*loss_percentage)[1:c]
  #make a list
  output= list("MaxLossPercentage"=loss_percentage[max(which(expected_loss[1:c]<=vec))],
               "MaxExpectedLoss"= expected_loss[max(which(expected_loss[1:c]<=vec))])
  return(output)
}

all_range_max_expected_loss<-function(expected_losses,vec_rebound_percentage,max_loss,cutoff)
{
  #same as the "max_expected_loss" function except "vec_rebound_percentage" is a vector and "expected_losses" is a matrix, where
  #each column represents a different stock
  
  outputs=list("ReboundRates"=vec_rebound_percentage,
               "MaxLossPercentages"= matrix(data=NA, nrow=length(vec_rebound_percentage), ncol=ncol(expected_losses)),
               "MaxExpectedLosses"= matrix(data=NA, nrow=length(vec_rebound_percentage), ncol=ncol(expected_losses)))#preallocation
  
  for (k in 1:ncol(expected_losses))
  {
    for (i in 1:length(vec_rebound_percentage))
    {
      max_recoverable_expected_loss <- max_expected_loss(expected_losses[,k],vec_rebound_percentage[i],max_loss,cutoff)
      outputs$MaxLossPercentages[i,k] <-  max_recoverable_expected_loss$MaxLossPercentage
      outputs$MaxExpectedLosses[i,k] <- max_recoverable_expected_loss$MaxExpectedLoss
    }
  }
  return(outputs)
}
