
#load data, all log returns


DOL.TO<- rev(getReturns('DOL.TO', freq = 'day', get = 'overlapOnly',start='2011-01-01', end = NULL)$full$DOL.TO$Adj.Close)

DOL.TO_r<-price2r(DOL.TO); plot(DOL.TO_r, type="l",main="returns")

DOL.TO_lr<-price2lr(DOL.TO); plot(DOL.TO_lr, type ="l",main = "logreturns")