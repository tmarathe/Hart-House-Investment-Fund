#Author: Tarang Marathe
library("PerformanceAnalytics")
library("matrixStats")

#market_prices_location = "Users/tarang/Documents/Quant/market_prices_nov7.csv"
#set work space
#dropbox_loc <- "C:/Users/kevinhu/Dropbox/"
model_loc <- "HHIF Quant/portfolio analysis"
#setwd(paste(dropbox_loc, model_loc, sep=""))


market_prices_df = read.csv("mp_metrics_since_inception.csv")  #CHOOSE FILE TO ANALYZE HERE
date_list = market_prices_df[, 1]
rownames(market_prices_df) = date_list #set the rownames to be the date of market returns
market_prices_df[, 1] = NULL #delete unnecessary date column

#benchmark_log_returns = c(0.000349484, 0.003768396, 0.005684325, -0.002833812) #hardcoded benchmark FIX THIS
portfolio_value = 10797997.22 # January 9th, 2015
futures_contract_cost = 250 # S&P futures contract cost
SP_index_TODAY = 2062.14 # January 9th, 2015
risk_free_rate = 0.0002 # 3 month treasury bill rate



weights_df = read.csv("metrics_weights_jan8.csv")
stock_names = weights_df[,1]
rownames(weights_df) = stock_names
weights_df[,1] = NULL

#normal_returns_df = data.frame(head(date_list, -1))
#for(j in 1:length(colnames(market_prices_df)))
#{
# i = 1
# returns_vector = c()
# while(i < length(market_prices_df[, j]))
# {
#   normal_return = market_prices_df[, j][i] / market_prices_df[, j][i+1]
#   #print(normal_return) #uncomment for debugging
#   normal_returns_vector = c(normal_returns_vector, normal_return)
#   i = i + 1
# }
# #print(normal_returns_vector) uncomment for debugging
# normal_returns_df[, colnames(market_prices_df)[j]] <- normal_returns_vector
#}



#initializing log returns df
log_returns_df = data.frame(head(date_list, -1))
for(j in 1:length(colnames(market_prices_df)))
{sd
  i = 1
  log_returns_vector = c()
  while(i < length(market_prices_df[, j]))
  {
  log_return = log(market_prices_df[, j][i] / market_prices_df[, j][i+1])
  #print(log_return) #uncomment for debugging
  log_returns_vector = c(log_returns_vector, log_return)
  i = i + 1
  }
  #print(log_returns_vector) uncomment for debugging
  log_returns_df[, colnames(market_prices_df)[j]] <- log_returns_vector
}
#cleanup for the log_returns_df data
rownames(log_returns_df) = head(date_list, -1)#date_list[1:4] #set the rownames to be the date of market returns
log_returns_df[, 1] = NULL #delete unnecessary date column
#print(log_returns_df)

benchmark_log_returns = log_returns_df[, 7]


#MANUAL VaR
#standard deviation of log returns for each stock
sd_vector = c()
for(k in 1:length(log_returns_df)){
  sd_vector = c(sd_vector, sapply(log_returns_df[k], sd))
}
VaR_pvec = seq(0.89, 0.99, by=0.01)
VaR_valvec = c()
for(p in VaR_pvec){
sd_times_pos = qnorm(p)*sd_vector*weights_df[, 2]
VaR_step1 = sd_times_pos %*% cor(log_returns_df)
VaR_final = sqrt(VaR_step1 %*% sd_times_pos)
VaR_valvec = c(VaR_valvec, VaR_final)
}
VaR_as_percent = -1*VaR_valvec / portfolio_value
plot(VaR_pvec, VaR_as_percent, type='l', main="One Day VaR Sensitivity Analysis of HHIC Portfolio",
     xlab="Confidence Interval", ylab="VaR as % of portfolio")


#Calculating Beta

stock_beta = c()
for(b in 1:length(log_returns_df)){
  stock_beta = c(stock_beta, abs(cov(log_returns_df[b], benchmark_log_returns) / var(benchmark_log_returns)))
}
weighted_beta = sum(weights_df[1]*stock_beta)


#ENTER REQUIRED BETA HERE BEFORE RUNNING
required_beta =  2.4
futures = ((required_beta - weighted_beta) * portfolio_value) / (futures_contract_cost * SP_index_TODAY)


#PRINT ALL METRICS
#VaR
cat("One Day VaR (USD): $", VaR_final[1])
#Beta
cat("Portfolio Beta:", weighted_beta)
#Futures contracts
if(futures > 0){
  cat("Long", round(futures, digits=0), "futures contracts.")
} else{
  cat("Short", round(abs(futures), digits=0), "futures contracts.")
}

#Active risk / tracking error
portfolio_vals = read.csv("portfolio_value_since_inception.csv")
port_dates = portfolio_vals[,1]
rownames(portfolio_vals) = port_dates
portfolio_vals[,1]= NULL

portfolio_returns = c()
for(m in 1:length(portfolio_vals[,1])-1)
{
  p_ret = (portfolio_vals[m,1] - portfolio_vals[m+1,1]) / portfolio_vals[m+1,1]
  portfolio_returns = c(portfolio_returns, p_ret)
}

benchmark_returns = c()
for(m in 1:length(portfolio_vals[,1])-1)
{
  b_ret = (portfolio_vals[m,2] - portfolio_vals[m+1,2]) / portfolio_vals[m+1,2]
  benchmark_returns = c(benchmark_returns, b_ret)
}

tracking_error = sqrt(sum((portfolio_returns - benchmark_returns)^2) / (length(benchmark_returns) -1))


#ALPHA
alpha = portfolio_returns[1] - (risk_free_rate + weighted_beta*(benchmark_returns[1] - risk_free_rate))


#Sharpe ratio
sharpe_ratio = (portfolio_returns[1] - risk_free_rate) / tracking_error


#Information ratio
info_ratio = (portfolio_returns[1] - benchmark_returns[1])/tracking_error


#PRINT EVERYTHING
cat("One Day VaR (USD): $", VaR_final[1])
cat("Portfolio Beta:", weighted_beta)
cat("Tracking Error is:", tracking_error*100, "%")
cat("Alpha is:", alpha*100,"%")
cat("The Sharpe Ratio is:", sharpe_ratio)
cat("The Information Ratio is: ", info_ratio)

#AUTOMATED VaR (Does not work yet!)
#VaR_vector = c()
#for(k in 1:length(log_returns_df)){
#  VaR_vector = c(VaR_vector, VaR(log_returns_df[k], p=0.95, method="historical"))
#}
