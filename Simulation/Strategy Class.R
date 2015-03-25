setwd("G:/Dropbox/HHIF Quant/Simulation")
library(stockPortfolio)
library(TTR)



setClass("Strategy",
         representation(strategyname="character",
                        strategy="function",
                        numstocks="numeric",
                        historical="data.frame",
                        position="numeric",
                        accountvalue="numeric",
                        storage="list"),
         prototype(strategyname="nothing",
                   strategy=function(strategy, curprices, curposition, curaccountvalue) rep(0, length(curposition)),
                   numstocks=5,
                   historical=data.frame(NULL),
                   position=numeric(5),
                   accountvalue=0,
                   storage=list()))


setMethod("show", "Strategy",
          function(object){
            return(object@strategyname)
          })