#Download Data#################
library(rvest)#for downloading wikipedia table
#can find tickers for other dates by changing old id
#Wikipedia history from 28 December 2015
oldid<-697200065
url<-paste("https://en.wikipedia.org/w/index.php?title=List_of_S%26P_500_companies&oldid=",oldid,sep="")


#download table based on:
#https://help.displayr.com/hc/en-us/articles/360003582875-How-to-Import-a-Wikipedia-Table-using-R
page = read_html(url)
my.table = html_node(page, ".wikitable")
my.table = html_table(my.table, fill = TRUE)


tickers<-my.table$`Ticker symbol`
names(tickers)<-my.table$Security
#take a look at the tickers
unname(tickers)