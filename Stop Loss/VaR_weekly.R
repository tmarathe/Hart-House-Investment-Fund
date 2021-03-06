#rolling R Break points weekly prices 2009 to current 

library(PerformanceAnalytics)

#import cp 
wp_cp <- rev(CP_WEEKLY[,7])
price2return <- function (prices)  diff(prices)/prices[1:(length(prices)-1)]

cp_r <- price2return (wp_cp)
cp_r_ts <-ts(cp_r)


chart.VaRSensitivity(cp_r_ts,
                     methods=c("HistoricalVaR", "GaussianVaR"),
                     colorset=bluefocus, lwd=2,
                     main = "Weekly VaR Sensitivity Analysis of CP")

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
period = 30
r_VaR_99 = c(rep(0,period -1 ),rolling_VaR(cp_r_ts,period,p = 0.99)) 
r_VaR_95 = c(rep(0,period -1),rolling_VaR(cp_r_ts,period,p = 0.95))  
r_VaR_75 = c(rep(0,period -1),rolling_VaR(cp_r_ts,period,p = 0.75))

#returns approach
plot(cp_r_ts,
     type = "l",
     xlab = "Time (daily)",
     ylab = "Returns", 
     col = "black",
     main = "Evolutions of Rolling VaR 95%, and 75%")
lines(r_VaR_99, col = "red")
lines(r_VaR_95, col = "orange")
lines(r_VaR_75, col = "blue")
abline(0,0)


#adding price VaRs

VaR2PriceVaR <-function (VaR, price)
{
  priceVaR <- price[2:length(price)] + price[2:length(price)] * VaR
  priceVaR
}

plot(wp_cp,
     type = "l",
     xlab = "Time (weekly)",
     ylab = "Prices",
     col = "black",
     main = "Rolling VaR BreakPoints")
lines(VaR2PriceVaR(r_VaR_99,wp_cp), col = "red") 
lines(VaR2PriceVaR(r_VaR_95,wp_cp), col = "orange")

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

plot(wp_cp,
     type = "l",
     xlab = "Time (weekly)",
     ylab = "Prices",
     col = "black",
     main = "Rolling 99% VaR BreakPoints")
lines(breakVaR(wp_cp,r_VaR_99), col = "red")






