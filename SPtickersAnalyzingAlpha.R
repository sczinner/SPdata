#from https://github.com/leosmigel/analyzingalpha/blob/master/2019-09-18-sp500-historical-components-and-changes/sp500_history.csv
changes<-read.csv("spcomponentchanges.csv")
changes$date<-as.Date(changes$date,"%b %d, %Y")

#checking if dates are sorted
all(order(changes$date,decreasing = T)==1:length(changes$date))

sum(changes[,"action"]=="added")
sum(changes[,"action"]=="removed")

nchanges<-nrow(changes)
tickers<-rev(unique(changes$ticker))
ntickers<-length(tickers)



# components<-matrix(0,nrow=nchanges,ncol=ntickers)
# colnames(components)<-tickers
# rownames(components)<-as.character(changes$date)
# 
# for(ii in rev(1:nchanges)){
#   if(ii!=nchanges){
#     components[ii,]=components[ii+1,]
#   }
#   change<-changes[ii,]
#   ticker<-change$ticker
#   action=change$action
#   if(action=="added"){
#     components[ii,which(tickers==ticker)]=1
#   }else if(action=="removed"){
#     components[ii,which(tickers==ticker)]=0
#   }
# }
# 
# getTickers1<-function(ii){
#   colnames(components)[which(components[ii,]==1)]
# }

dates<-rev(unique(changes$date))
ndates<-length(dates)
components2<-matrix(0,nrow=ndates,ncol=ntickers)
colnames(components2)<-tickers
rownames(components2)<-as.character(dates)

count=1
lastdate<-dates[1]
for(ii in rev(1:nchanges)){
  change<-changes[ii,]
  ticker<-change$ticker
  action=change$action
  date=change$date
  
  if(date!=lastdate){
    count=count+1
    lastdate=date
    components2[count,]<-components2[count-1,]
  }
  
  if(action=="added"){
    components2[count,which(tickers==ticker)]=1
  }else if(action=="removed"){
    components2[count,which(tickers==ticker)]=0
  }
}

getTickers2<-function(ii){
  colnames(components2)[which(components2[ii,]==1)]
}

getTickers2(404)

#date as "YYYY-mm-dd" or date
getTickersDate<-function(date){
  if(is.character(date)){
    date=as.Date(date,"%Y-%m-%d")
  }
  ii<-max(which(date>as.Date(rownames(components2),"%Y-%m-%d")))
  return(getTickers2(ii))
  
}
date="2019-12-01"
tickers<-getTickersDate(date)

rowSums(components2)