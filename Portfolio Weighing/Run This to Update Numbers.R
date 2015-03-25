#Firstly update the tickers and target prices in StockTargetPrices.csv
#Then change the file path below to your dropbox location:
dropbox_loc <- "C:/Users/kevinhu/Dropbox/"
#And run all lines in this file (including the line above)
model_loc <- "HHIF Quant/Portfolio Weighing"
setwd(paste(dropbox_loc, model_loc, sep=""))
source('ProduceReturnandVolatility.R')