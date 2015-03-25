#Holdings as of 2/16/2015
#We do not change holdings frequently so I will only update the holdings if we have new transactions 
#please set work space appropriately 
#after running the code, call portfolio_value,cash and portfolio_weight 



setwd("C:/Users/Kevin Hu/Desktop/Dropbox/HHIF Quant/Portfolio/Weight Calculations")

#mannually input the symbols of the tickers that are not included in the SP500 and NOT including the sector ETFs
SP500_out<-c("CAS.TO","VNQ")

library(quantmod)
###########loading data##########
data<- read.csv("Weights.csv", header=T,stringsAsFactors=F) #change everything to number
SP500companies <- read.csv("tickers.csv", header=T) 
portfolio_tickers<-data$Ticker
portfolio_positions<-data$Position
portfolio_costs<-data$Cost


#####SP500 weights ###########
Symbol <- as.character(SP500companies$Ticker) #S&P 500 tickers
what_metrics <- yahooQF(c("Last Trade (Price Only)","Market Capitalization"))

SP500_data <- getQuote(Symbol, what = what_metrics)
aux <- as.character(SP500_data$"Market Capitalization")
SP500_marketcap <- as.numeric(substr(aux, start=1, stop=nchar(aux)-1)) #get rid of the trash, selecting only the number
dig <- substr(aux, start=nchar(aux), stop=nchar(aux))  #hold the multiplier

SP500_marketcap[which(is.na(SP500_marketcap))]=mean(na.omit(SP500_marketcap))*1e9

for(i in 1:length(SP500_marketcap)){
  if(dig[i] == "B"){
    SP500_marketcap[i] <- SP500_marketcap[i]*1e9
  }
  if(dig[i] == "M"){
    SP500_marketcap[i] <- SP500_marketcap[i]*1e6
  }
}
SP500_weights<- SP500_marketcap/sum(SP500_marketcap)

########portfolio value################
portfolio_data<- getQuote(portfolio_tickers, what = what_metrics)
portfolio_value<-sum(portfolio_positions*portfolio_data$Last)


########portfolio market caps################
#split into 3 groups SP500_in=picks in S&P500, SP500_out=picks not in S&P500, ETF= etf holdings 

SP500_in_ind<- which(portfolio_tickers %in% Symbol) 
SP500_in<- portfolio_tickers[SP500_in_ind] #symbols of stock in portfolio that are in the SP500

#SP500_out is user input
SP500_out_ind<- which(portfolio_tickers %in% SP500_out)

not_ETF<-c(SP500_in,SP500_out)
ETF_ind<- which(!portfolio_tickers %in% not_ETF)
ETF<-portfolio_tickers[ETF_ind] 

#calculations
O<-1 #initialize 
for (i in 1:length(SP500_out_ind))
{
  O[i]= portfolio_positions[SP500_out_ind[i]]*portfolio_data$Last[SP500_out_ind[i]]
}

I<-1 #initialize 
for (i in 1:length(SP500_in_ind))
{
  I[i]= portfolio_positions[SP500_in_ind[i]]*portfolio_data$Last[SP500_in_ind[i]]
}

 
E<-1 #initialize 
for (i in 1:length(ETF_ind))
{
  E[i]= portfolio_positions[ETF_ind[i]]*portfolio_data$Last[ETF_ind[i]]
}


SP500_out_marketcap<-sum(O)
ETF_marketcap<- sum(E)
SP500_in_marketcap<- sum(I)

#check
(SP500_out_marketcap + ETF_marketcap + SP500_in_marketcap)== portfolio_value

##############portfolio weights################

#pre-allocation
portfolio_weights<- matrix(data=NA, nrow= (length(SP500_out)+length(Symbol)), ncol=2)
holdings<-c(Symbol,SP500_out)
portfolio_weights[,1] <- holdings


#SP500_out
SP500_out_weights<-rep(0,length(SP500_out))
O_ind_h<-which(holdings %in% SP500_out)
O_ind_p<-which(portfolio_tickers %in% SP500_out)

for (i in 1:length(SP500_out_weights))
{
  SP500_out_weights[i]<- portfolio_positions[O_ind_p[i]]*portfolio_data$Last[O_ind_p[i]]/portfolio_value
  portfolio_weights[O_ind_h[i],2]<-SP500_out_weights[i]
}


#ETFs
ETF_weights<-rep(0,length(Symbol))
ETF_ind_h<-which(holdings %in% Symbol)


ETF_weights<- SP500_weights * (ETF_marketcap/portfolio_value)
for (i in 1:length(ETF_weights))
{
  portfolio_weights[ETF_ind_h[i],2]<-ETF_weights[i]
}


#ETFs+ SP500_in
SP500_in_weights<-rep(0,length(SP500_in))
I_ind_h<-which(holdings %in% SP500_in)
I_ind_p<-which(portfolio_tickers %in% SP500_in)

for (i in 1:length(SP500_in_weights))
{
  SP500_in_weights[i]<- (portfolio_positions[I_ind_p[i]]*portfolio_data$Last[I_ind_p[i]]/portfolio_value) + ETF_weights[I_ind_h[i]]
  portfolio_weights[I_ind_h[i],2]<-SP500_in_weights[i]
}

#check should round to 1 
sum(as.numeric(portfolio_weights[,2]))

#Cash
total_cost<- sum(portfolio_costs)
cash<-10000000+total_cost

#sorting 
portfolio_weights_f<- data.frame(portfolio_weights)
sorted_portfolio_weights<- portfolio_weights[order(portfolio_weights_f$X2,decreasing=TRUE),]

#portfolio_value
#cash
#sorted_portfolio_weights

write.csv(data.frame(sorted_portfolio_weights),"ranked_holdings.csv")