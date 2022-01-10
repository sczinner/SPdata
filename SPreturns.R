source(file="SPtickersfromwikipedia.R")#this should load tickers from 2015-12-28
library(quantmod)#for downloading yahoo finance daily prices
#Download Data#################

pricedata<-list()

date="2016-01-01"#should use a date close to the date of historic S&P tickers
done=FALSE#continue until tries downloading all and doesn't download a single one
while(!done){
  done=TRUE
  for(index in 1:length(tickers)){
    
    a_symbol<-tickers[index]
    a_name<-names(tickers)[index]
    
    if(is.null(pricedata[[a_symbol]])){try({
      print(index)
      options("getSymbols.warning4.0"=FALSE)
      options("getSymbols.yahoo.warning"=FALSE)
      pricedata[[a_symbol]]<-getSymbols(a_symbol, from = date,auto.assign = FALSE)
      done=FALSE
      print(paste("downloaded",a_symbol))
    })
      
    }
    
  }
}

#Data cleaning#################
prices<-lapply(pricedata,function(x)x[,6])
len<-summary(sapply(prices,length))[2]
#gets rid of stock data that is shorter than the rest
prices = prices[-which(sapply(prices, function(x)length(x)<len))]
prices<-as.data.frame(prices)

#gets rid of stocks that have too many NA
nremove<-summary(apply(prices,2,function(x)sum(is.na(x))))[2]

indremove<-which(apply(prices,2,function(x)sum(is.na(x)))>nremove)
prices<-prices[,-indremove]

max(apply(prices,2,function(x)sum(is.na(x))))
#get rid of any *days* with incomplete data
prices<-prices[complete.cases(prices),]

length(which(!complete.cases(prices)))
datetext<-rownames(prices[,1:2])

#returns##############

returns<-apply(prices,2,function(x)diff(log(x)))
returndate<-as.Date(datetext[-1])

#This should get rid of stocks whose returns are identical (stocks which have
#different classes of shares both listed in SP500)
A<-cor(returns)
lim<-0.95
while(max(A[lower.tri(A)])>lim){
  rem<-which(A==max(A[lower.tri(A)]),arr.ind = T)
  rem
  returns<-returns[,-rem[1]]
  A<-cor(returns)
}

write.csv(returns,paste("SPreturns",date,tail(returndate,1),".csv",sep=""))