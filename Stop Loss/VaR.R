library(PerformanceAnalytics)

#import cp 
dp_cp <- rev(CP[,7])
price2return <- function (prices)  diff(prices)/prices[1:(length(prices)-1)]

cp_r <- price2return (dp_cp)
cp_r_ts <-ts(cp_r)


chart.VaRSensitivity(cp_r_ts,
                     methods=c("HistoricalVaR", "GaussianVaR"),
                     colorset=bluefocus, lwd=2,
                     main = "Daily VaR Sensitivity Analysis of CP")

rolling_VaR <- function (data, period, p)
{
  if (period < 1)
    stop("invalid number of period")
  mvar <- rep(0, length(data) - period + 1 ) # preallocation 
  for (i in period:length(data)) {
    mvar[(i- period + 1)] <- VaR(data[(i- period + 1):i],p, method="historical") 
  }
  mvar
}

#weekly rolling VaR
r_VaR_95 = c(rep(0,29),rolling_VaR(cp_r_ts,30,p = 0.95))  
r_VaR_75 = c(rep(0,29),rolling_VaR(cp_r_ts,30,p = 0.75))

#returns approach
plot(cp_r_ts,
     type = "l",
     xlab = "Time (daily)",
     ylab = "Returns", 
     col = "black",
     main = "Evolutions of Rolling VaR 95%, and 75%")
lines(r_VaR_95, col = "red")
lines(r_VaR_75, col = "orange")
abline(0,0)


#price alert approach 

#finding time where the var breaks 
breakVaR <- function (price,VaR)
{
  returns <- price2return(price)
  doesbreak <- returns < VaR
  doesbreak <- c(0,doesbreak)
  doesbreakprice<- price * doesbreak
  
  doesbreakprice  
}





#adding price vars

VaR2PriceVaR <-function (VaR, price)
{
  priceVaR <- price[2:length(price)] + price[2:length(price)] * VaR
  priceVaR
}

lines(VaR2PriceVaR(r_VaR_95,dp_cp), col = "red")  #add zero for current time the rule should be previous days 
lines(VaR2PriceVaR(r_VaR_75,dp_cp), col = "orange")

