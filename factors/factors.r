library(quantmod)

#Read data about the S&P500 companies
pathsp500 <- "/Users/hudsonps/Dropbox/factors/tickers.csv"
outputpath <- "/Users/hudsonps/Dropbox/factors/fdata.txt"
pathextracompanies <- "/Users/hudsonps/Dropbox/factors/extratickersportfolio.csv"
pathportfoliodata <- "/Users/hudsonps/Dropbox/factors/portfoliodata.csv"


SP500companies <- read.csv(pathsp500, header=T)
extracompanies <- read.csv(pathextracompanies, header=T)
portfoliodata <- read.csv(pathportfoliodata)

numbercompanies <- 502 

numberextracompanies <- 7 #additional companies that are not PART of S&P500, but part of the benchmark
#Companies
#DGC.TO, MHH, TD, SPR, SNY, MGA, ATD-B.TO    

Symbol <- as.character(SP500companies$Ticker) #S&P 500 tickers
Symbolextra <- as.character(extracompanies$Ticker) #additional companies tickers
Symbol <- c(Symbol, Symbolextra) #combines the tickers together

Sector <- SP500companies$GICS.Sector  #S&P500 companies sectors
Sectorextra <- extracompanies$GICS.Sector #additional companies sectors
Sector <- c(as.character(Sector), as.character(Sectorextra)) #combines the tickers together
Sector <- as.factor(Sector)

Portfoliocompanies <- as.character(portfoliodata$Ticker) #contains portfolio companies tickers
Portfoliocompanies <- Portfoliocompanies[Portfoliocompanies!=""] #to get rid of empty elements
numberportfoliocompanies <- length(Portfoliocompanies) 

#metrics to be download from yahoo:
what_metrics <- yahooQF(c("Change in Percent", "Dividend Yield", 
                          "Market Capitalization"))


marketcap <- 1  #This is just to initialize the variable marketcap
dig <- 1  #This is just to initialize the variable dig, which holds the multiplier (usually a factor of billion)
yield <- 1

                                        #Benchmark (S&P500) stuff

#Downloading the FINANCIAL DATA
factor_data <- getQuote(Symbol, what = what_metrics)


#Obtaining the RETURN
return <- factor_data$"% Change"
return <- as.numeric(substr(return, start=1, stop=nchar(return)-1))

#Obtaining the MARKET CAP
aux <- as.character(factor_data$"Market Capitalization")
marketcap <- as.numeric(substr(aux, start=1, stop=nchar(aux)-1)) #get rid of the trash, selecting only the number
dig <- substr(aux, start=nchar(aux), stop=nchar(aux))  #hold the multiplier

#There has been problems collecting ALLE marketcap from yahoo, so I am putting this here by hand!
                                        #ALLE data corresponds to Symbol[310]
marketcap[which(is.na(marketcap))]=0

for(i in 1:(numbercompanies+numberextracompanies)){
    if(dig[i] == "B"){
        marketcap[i] <- marketcap[i]*1e9
    }
    if(dig[i] == "M"){
        marketcap[i] <- marketcap[i]*1e6
    }
}



#Obtaining the YIELD
yield <- factor_data$"Dividend Yield" 
for(i in 1:(numbercompanies+numberextracompanies)){
    if(yield[i] == "N/A"){  #if yield is not available, set it to zero
    yield[i] <- 0
}
}
yield <- as.numeric(yield)   #transforms yield into a number

#Obtaining the BENCHMARK WEIGHT
benchmarkweight <- marketcap/sum(marketcap[1:numbercompanies])
 #To calculate the benchmark weight we only consider the companies included in the S&P500 index, i.e., numbercompanies.
for(i in 503:509){
    benchmarkweight[i] <- 0
}


#Obtaining the PORTFOLIO WEIGHT
#The portfolio is trickier because there are investments on sectors
#These must be translated into weights for individual companies

portfolioweight <- rep(0, numbercompanies+numberextracompanies)

ConsDis_first <- 1
ConsDis_last <- 83

ConsSta_first <- 84
ConsSta_last <- 123

Energy_first <- 124
Energy_last <- 167

Financials_first <- 168
Financials_last <- 253

Health_first <- 254
Health_last <- 308

Ind_first <- 309
Ind_last <- 374

Tech_first <- 375
Tech_last <- 436

Materials_first <- 437
Materials_last <- 466

Utilities_first <- 473
Utilities_last <- 502

ConsDis_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLY")] #XLY
ConsSta_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLP")] #XLP
Energy_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLE")] #XLE
Financials_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLF")] #XLF
Health_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLV")] #XLV
Ind_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLI")] #XLI 
Tech_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLK")] #XLK
Materials_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLB")] #XLB
Utilities_weight_portfolio <- portfoliodata$SectorWeight[which(portfoliodata$SectorTickers=="XLU")] #XLU


#Consumer Discretionary Sector: runs from Symbol[1] to Symbol[83]; XLY
ConsDis_weight_benchmark <- sum(benchmarkweight[ConsDis_first:ConsDis_last]) #Consumer discretionary sector weight for S&P500
for(i in ConsDis_first:ConsDis_last){
    portfolioweight[i] <- benchmarkweight[i]*ConsDis_weight_portfolio/ConsDis_weight_benchmark
    #This loop normalizes the weights so that the weight of the consumer discretionary sector for the portfolio is the chosen for the benchmark
}

#Consumer Staple Sector: runs from Symbol[84] to Symbol[123]; XLP
ConsSta_weight_benchmark <- sum(benchmarkweight[ConsSta_first:ConsSta_last]) #Consumer staple sector weight for S&P500
for(i in ConsSta_first:ConsSta_last){
    portfolioweight[i] <- benchmarkweight[i]*ConsSta_weight_portfolio/ConsSta_weight_benchmark
    #This loop normalizes the weights so that the weight of the consumer staple sector for the portfolio is the chosen for the benchmark
}

#Energy Sector: runs from Symbol[124] to Symbol[167]; XLE
Energy_weight_benchmark <- sum(benchmarkweight[Energy_first:Energy_last]) #Energy sector weight for S&P500
for(i in Energy_first:Energy_last){
    portfolioweight[i] <- benchmarkweight[i]*Energy_weight_portfolio/Energy_weight_benchmark
    #This loop normalizes the weights so that the weight of the energy sector for the portfolio is the chosen for the benchmark
}

#Financials Sector: runs from Symbol[168] to Symbol[253]; XLF
Financials_weight_benchmark <- sum(benchmarkweight[Financials_first:Financials_last]) #Financials sector weight for S&P500
for(i in Financials_first:Financials_last){
    portfolioweight[i] <- benchmarkweight[i]*Financials_weight_portfolio/Financials_weight_benchmark
    #This loop normalizes the weights so that the weight of the tech sector for the portfolio is the chosen for the benchmark
}

#Health Sector: runs from Symbol[254] to Symbol[308]; XLV
Health_weight_benchmark <- sum(benchmarkweight[Health_first:Health_last]) #Health sector weight for S&P500
for(i in Health_first:Health_last){
    portfolioweight[i] <- benchmarkweight[i]*Health_weight_portfolio/Health_weight_benchmark
    #This loop normalizes the weights so that the weight of the health sector for the portfolio is the chosen for the benchmark
}

#Industrials Sector: runs from Symbol[309] to Symbol[374]; XLI
Ind_weight_benchmark <- sum(benchmarkweight[Ind_first:Ind_last]) #Industrials sector weight for S&P500
for(i in Ind_first:Ind_last){
    portfolioweight[i] <- benchmarkweight[i]*Ind_weight_portfolio/Ind_weight_benchmark
    #This loop normalizes the weights so that the weight of the industrials sector for the portfolio is the chosen for the benchmark
}

#Tech Sector: runs from Symbol[375] to Symbol[436]; XLT
Tech_weight_benchmark <- sum(benchmarkweight[Tech_first:Tech_last]) #Tech sector weight for S&P500
for(i in Tech_first:Tech_last){
    portfolioweight[i] <- benchmarkweight[i]*Tech_weight_portfolio/Tech_weight_benchmark
    #This loop normalizes the weights so that the weight of the tech sector for the portfolio is the chosen for the benchmark
}

#Materials Sector: runs from Symbol[437] to Symbol[467]; XLB
Materials_weight_benchmark <- sum(benchmarkweight[Materials_first:Materials_last]) #Materials sector weight for S&P500
for(i in Materials_first:Materials_last){
    portfolioweight[i] <- benchmarkweight[i]*Materials_weight_portfolio/Materials_weight_benchmark
    #This loop normalizes the weights so that the weight of the materials sector for the portfolio is the chosen for the benchmark
}

#Utilities Sector: runs from Symbol[473] to Symbol[502]; XLU
Utilities_weight_benchmark <- sum(benchmarkweight[Utilities_first:Utilities_last]) #Utilities sector weight for S&P500
for(i in Utilities_first:Utilities_last){
    portfolioweight[i] <- benchmarkweight[i]*Utilities_weight_portfolio/Utilities_weight_benchmark
#This loop normalizes the weights so that the weight of the utilities sector is the chosen for the benchmark
}

#Now we must still include the specific companies

for (i in 1:numberportfoliocompanies){
    aux <- which(Symbol == Portfoliocompanies[i]) #contains the portfolio company number now
    portfolioweight[aux] <- portfolioweight[aux] + portfoliodata$Weight[i]
}

#Generating the output
col.names <- c("name", "return", "date", "sector", "marketcap", "yield", "portfolio", "benchmark")

date <- rep(0, numbercompanies+numberextracompanies)
for(i in 1:(numbercompanies+numberextracompanies)){
    date[i] <- Sys.Date()
}
date <- as.Date(date)


fdata <- data.frame(Symbol, return, date, Sector, marketcap, yield, portfolioweight, benchmarkweight)
colnames(fdata) <- col.names

write.table(fdata, file = outputpath, quote = FALSE, sep = " ",
            eol = "\n", na = "NA", dec = ".",
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")











