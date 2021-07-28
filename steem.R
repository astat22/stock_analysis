library(httr)
library(XML)
results = GET("https://en.wikipedia.org/wiki/List_of_cryptocurrencies")
doc = readHTMLTable(doc=content(results, "text"))
View(doc[1])




response = fromJSON('https://www.cryptocompare.com/api/data/coinlist')
df = data.table::rbindlist(response$Data, fill=TRUE)
View(df)

sort(df$FullName,decreasing=TRUE)
df[df$FullName=="Steem (STEEM)"]

library(quantmod)
getFX("STEEM/USD",from="2005-01-01")


STEEM = GET("https://coinmarketcap.com/currencies/steem-dollars/historical-data/?start=20130428&end=20170716")
docSTEEM = readHTMLTable(doc=content(STEEM, "text"))
View(docSTEEM[2])


###Analiza techniczna





library(jsonlite)
library(data.table)
library(quantmod) #Quantitative Financial Modelling & Trading Framework for R
library(TTR) #Technical Trading Rules
library(XML)
library(RCurl)
library(rlist)
library(tidyquant)
theurl <- getURL("https://coinmarketcap.com/currencies/steem-dollars/historical-data/?start=20130428&end=20170716",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
steemdata<-tables[[which.max(n.rows)]]
stdf <- data.frame(steemdata,stringsAsFactors = FALSE)
sessionInfo()
Sys.setlocale("LC_TIME", "English")
stdf[,1]<-as.Date(stdf[,1],"%h %d, %Y")
#stdf[,2]<-as.numeric(stdf[,2])
stdf[,6]<-as.numeric(gsub(",", "", stdf[,6]))
stdf[,7]<-as.numeric(gsub(",", "", stdf[,7]))
stdf[is.na(stdf[,7]),7]<-0

stxts<-as_xts(stdf, date_col = Date)
storage.mode(stxts) <- "numeric"

chartSeries(stxts, type="candlesticks",up.col="green",dn.col="red")
chartSeries(to.weekly(stxts), type="candlesticks",up.col="green",dn.col="red") #agregacja tygodniowa
chartSeries(to.monthly(stxts), type="candlesticks",up.col="green",dn.col="red") #agregacja miesięczna

chartSeries(stxts, type="line",up.col="green",dn.col="red")
chartSeries(to.weekly(stxts), type="line",up.col="green",dn.col="red") #agregacja tygodniowa
chartSeries(to.monthly(stxts), type="line",up.col="green",dn.col="red") #agregacja miesięczna



########################po 17 maja 2017
theurl <- getURL("https://coinmarketcap.com/currencies/steem-dollars/historical-data/?start=20170518&end=20170719",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
steemdata<-tables[[which.max(n.rows)]]
stdf <- data.frame(steemdata,stringsAsFactors = FALSE)
#sessionInfo()
Sys.setlocale("LC_TIME", "English")
stdf[,1]<-as.Date(stdf[,1],"%h %d, %Y")
#stdf[,2]<-as.numeric(stdf[,2])
stdf[,6]<-as.numeric(gsub(",", "", stdf[,6]))
stdf[,7]<-as.numeric(gsub(",", "", stdf[,7]))
stdf[is.na(stdf[,7]),7]<-0

stxts<-as_xts(stdf, date_col = Date)
storage.mode(stxts) <- "numeric"

chartSeries(stxts, type="candlesticks",up.col="green",dn.col="red")
chartSeries(to.weekly(stxts), type="candlesticks",up.col="green",dn.col="red") #agregacja tygodniowa
chartSeries(to.monthly(stxts), type="candlesticks",up.col="green",dn.col="red") #agregacja miesięczna

chartSeries(stxts, type="line",up.col="green",dn.col="red")
chartSeries(to.weekly(stxts), type="line",up.col="green",dn.col="red") #agregacja tygodniowa
chartSeries(to.monthly(stxts), type="line",up.col="green",dn.col="red") #agregacja miesięczna

chartSeries(stxts, type="matchsticks",up.col="green",dn.col="red")
chartSeries(stxts, type="bars",up.col="green",dn.col="red")
chartSeries(stxts, type="auto",up.col="green",dn.col="red")

chartSeries(stxts, type = "line", up.col = "green", dn.col="red", line.type="l",show.grid = TRUE)
addMACD(fast=12,slow=26,signal=1)
grid(nx=20, ny=1, lty=2, lwd=0.1)


