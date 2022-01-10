source(file="SPtickersfromwikipedia.R")#this should load tickers from 2015-12-28
library(yfinance)#for downloading yahoo finance fundamental data
#Download Data#################

fundamentaldata<-list()

date1="2016-01-01"#should use a date close to the date of historic S&P tickers
done=FALSE#continue until tries downloading all and doesn't download a single one
while(!done){
  done=TRUE
  for(index in 1:length(tickers)){
    
    a_symbol<-tickers[index]
    a_name<-names(tickers)[index]
    
    if(is.null(fundamentaldata[[a_symbol]])){try({
      print(index)
      fundamentaldata[[a_symbol]]<-list(marketcap=get_price(a_symbol),
                                        financials=get_financials(a_symbol),
                                        summary=get_summaries(a_symbol))
      done=FALSE
      print(paste("downloaded",a_symbol))
    })
      
    }
    
  }
}
date2<-fundamentaldata[[1]]$marketcap$date
save(fundamentaldata,file=paste("SPfundamentaldata",date1,date2,".Rdata",sep=""))
