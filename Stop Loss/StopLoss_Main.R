setwd("C:/Users/kevinhu/Dropbox/HHIF Quant/Stop Loss")#set work space
source('StopLoss_Functions.R')

#to play around with different stocks just set "stock" to be different
#to access the data just type "lossdata" 
#to see the loss percentages type: lossdata$LossPercentages
#to see rebound perecentages type:lossdata$ReboundRate
#to see the expected losses type: lossdata$ExpectedLoss


#PRODUCE EXCEL SHEET
look_back<- 45 #"loss_percentage" within 30 days 
look_forward<-45 #rebounded to the "rebound percentag" in the next "look_forward" days 
filter<-5
max_loss<-0.2#if you set this too high, you will get warnings due to insufficient data

stocks<- read.csv("Tickers.csv", header=T, stringsAsFactors=F)$Tickers
all_lossdata<-all_loss_data(stocks,look_back,look_forward,filter,max_loss)
max_recoverable_expected_losses<-all_range_max_expected_loss(all_lossdata$ExpectedLosses,vec_rebound_percentage=seq(0.5,0.8,by=0.05),
                                                       max_loss,cutoff=0.1)

df<-t(data.frame(max_recoverable_expected_losses))
write.csv(df,"ExpectedLosses.csv")

#ONE STOCK STATISTICS AND GRAPHS
stock<-"AAPL"

#if you just want the data just call loss data
lossdata<-loss_data(stock,look_back,look_forward,filter,max_loss)

#plotting
#rebound percentages 
plot(lossdata$LossPercentages,lossdata$ReboundRate,type="l",
     main= lossdata$Ticker,
     xlab="Loss Percentage",
     ylab="Rebound Percentage")
legend('topright', legend = c(paste("Look Back Period:", look_back),
                            paste("Look Forward Period:",look_forward)))
grid()


#expected loss
plot(lossdata$LossPercentages,lossdata$ExpectedLoss,type="l",
         main= lossdata$Ticker,
         xlab="Loss Percentage",
         ylab="Expected Loss")
abline(a=0,b=0.5,col="red",lty=2)
abline(a=0,b=1,col="red")
legend('topleft', legend = c(paste("Look Back Period:", look_back),
                            paste("Look Forward Period:",look_forward)))

grid()




 