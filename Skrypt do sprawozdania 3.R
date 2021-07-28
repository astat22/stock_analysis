############################# Analiza techniczna #####################################################
#duża część kodu to zmodyfikowany kod z referatu o Analizie Technicznej; w gruncie rzeczy            #
#jego sens polegał na wyjaśnieniu zastosowań funkcyj zaimpelentowanych w R,                          #
#dlatego nie uważam, aby dostosowanie parametrów używanych funkcyj na licencji GPL do własnej analizy# 
#naruszało czyjeś prawa autorskie                                                                    #
######################################################################################################

library(quantmod)
library(TTR)
library(forecast)
library(stats)
library(fArma)
library(signal)
library(itsmr)
library(splines)
library(tseries)

#### Wczytywanie danych: Dow Chemical Company ####

getSymbols(Symbols="DOW", src = "yahoo")

## Wykresy ##

chartSeries(DOW, type="line",up.col="green",dn.col="red")
chartSeries(DOW, type="candlesticks",up.col="green",dn.col="red")
chartSeries(to.weekly(DOW), type="candlesticks",up.col="green",dn.col="red") #agregacja tygodniowa
chartSeries(to.monthly(DOW), type="candlesticks",up.col="green",dn.col="red") #agregacja miesięczna
chartSeries(to.monthly(DOW), type="matchsticks",up.col="green",dn.col="red")
chartSeries(to.monthly(DOW), type="bars",up.col="green",dn.col="red")
chartSeries(to.monthly(DOW), type="auto",up.col="green",dn.col="red")
chartSeries(DOW, type="auto",up.col="green",dn.col="red")

### MACD ###

chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l",show.grid = TRUE)
addMACD(fast=12,slow=26,signal=9)
grid(nx=20, ny=1, lty=2, lwd=0.1)

chartSeries(to.monthly(DOW), type = "candlesticks", up.col="green", dn.col="red", line.type="l",show.grid = TRUE)
addMACD(fast=2,slow=4,signal=4)
grid(nx=20, ny=1, lty=2, lwd=0.1)

### STS ###

#tygodniowo
chartSeries(to.weekly(DOW), type = "candlesticks", up.col="black", dn.col="red", line.type="l", theme=chartTheme(4))
addTA(DOW[,4],on=1,col="white",lwd=1)
SMI = SMI(to.weekly(DOW)[,2:4], n=14, nFast = 2, nSlow = 4, nSig = 3)
addTA(SMI,on=NA,col=c("blue","red"),lwd=c(1,1), lty=c(1,1))
grid(nx=20, ny=1, lty=2, lwd=0.1, col="black")

#miesięcznie
chartSeries(to.monthly(DOW), type = "candlesticks", up.col="green", dn.col="red", line.type="l", theme=chartTheme(4))
addTA(DOW[,4],on=1,col="white",lwd=1)
SMI = SMI(to.monthly(DOW)[,2:4], n=14, nFast = 2, nSlow = 4, nSig = 3)
addTA(SMI,on=NA,col=c("blue","red"),lwd=c(1,1), lty=c(1,1))
grid(nx=20, ny=1, lty=2, lwd=0.1, col="black")

### Wstęga Bollingera ###

n=10 
sd=2 
#dziennie
chartSeries(DOW, type = "line", up.col="grey", dn.col="red", line.type="l")
addTA(DOW[,4],on=1,col="grey",lwd=1)
BB = BBands(DOW[,2:4],n=n,sd=sd) #n=20, sd=2
addTA(BB,on=1,col=c("red","white","green","yellow"),lwd=c(1,1,1,1))
#można też addBBands z quantmod

#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="grey", dn.col="red", line.type="l")
addTA(DOW[,4],on=1,col="grey",lwd=1)
BB = BBands(to.weekly(DOW)[,2:4],n=n,sd=sd)
addTA(BB,on=1,col=c("red","white","green","yellow"),lwd=c(1,1,1,1))

#miesięcznie
chartSeries(to.monthly(DOW), type = "line", up.col="grey", dn.col="red", line.type="l")
#addTA(DOW[,4],on=1,col="grey",lwd=1)
BB = BBands(to.monthly(DOW)[,2:4],n=n,sd=sd)
addTA(BB,on=1,col=c("red","white","green","green"),lwd=c(1,1,1,1))

### ATR ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addATR()
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addATR()
#miesięcznie
chartSeries(to.monthly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addATR()

### Zmienność Chaikina ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
cV = chaikinVolatility(DOW)
addTA(cV,col="red",lwd=1)
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
cV = chaikinVolatility(to.weekly(DOW))
addTA(cV,col="red",lwd=1)
#miesięcznie
chartSeries(to.monthly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
cV = chaikinVolatility(to.monthly(DOW))
addTA(cV,col="red",lwd=1)

### Indykator Aroon ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addAroon()
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addAroon()
#miesięcznie
chartSeries(to.monthly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addAroon()

### AD Chaikina ###

chartSeries(DOW, type = "line", up.col="white", dn.col="red", line.type="l", theme=chartTheme(3))
addChAD()
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="white", dn.col="red", line.type="l", theme=chartTheme(3))
addChAD()

### CMF ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addCMF()
grid(nx=20, ny=1, lty=2, lwd=0.1, col="white")
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addCMF()
grid(nx=20, ny=1, lty=2, lwd=0.1, col="white")

### MFI ###

chartSeries(to.weekly(DOW), type = "bars", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addMFI()
grid(nx=20, ny=1, lty=2, lwd=0.1, col="white")

chartSeries(to.monthly(DOW), type = "bars", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addMFI()
grid(nx=20, ny=1, lty=2, lwd=0.1, col="white")

### ROC ###

chartSeries(DOW, type = "line", up.col="green", line.type="l", theme=chartTheme(3))
addROC(n=7)
#tygodniowo
chartSeries(to.weekly(DOW), type = "matchsticks", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addROC()

### CCI ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addCCI()
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addCCI()
#miesięcznie
chartSeries(to.monthly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addCCI()

### RSI ###

chartSeries(DOW, type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
grid(nx=20, ny=1, lty=2, lwd=0.1, col="grey")
addRSI()
#tygodniowo
chartSeries(to.weekly(DOW), type = "line", up.col="green", dn.col="red", line.type="l", theme=chartTheme(3))
addRSI()
grid(nx=20, ny=1, lty=2, lwd=0.1, col="grey")


############################ globtemp ###########################################

#wczytywanie
g.temp.z.pliku<- read.table("./globtemp.dat")
gtemp <- ts(g.temp.z.pliku, start=c(1856), frequency=1)
plot(gtemp,main="Odchylenia od średniej \n rocznej temperatury globalnej",xlab="Rok", ylab="")

#poprawianie szeregu.
gtempdiff<-diff(gtemp,lag=1,differences=1)
plot(gtempdiff)       #wariancja wyglada na jednorodna

#jednorodnosc wariancji. dla kazdej dekady (opoznienie=10) [t,t+10], [t+1,t+11],...,[t+i,t+10+i] wyznaczamy osobno wariancje
jednorodnosc<-function(X,opoznienie)
{
  wektor<-as.vector(X)
  n<-length(wektor)
  wariancje<-c(1:(n-opoznienie))
  for(i in 1:(n-opoznienie))
  {
    wariancje[i]<-var(wektor[i:(i+opoznienie)])
  }
  plot(wariancje,col="blue",type="line",xlab="Lata od 1856",ylab="Wariancja")
  sigma<-var(wariancje)
  sigma
}
jednorodnosc(gtempdiff,30)

jednorodnosc.rozlaczna<-function(X,opoznienie)
{
  wektor<-as.vector(X)
  n<-length(wektor)
  wariancje<-c(1:(n/opoznienie))
  for(i in 1:(n/opoznienie))
  {
    wariancje[i]<-var(wektor[(opoznienie*(i-1)):(opoznienie*i)])
  }
  plot(wariancje,col="blue",type="line",xlab="Lata od 1856",ylab="Wariancja")
  sigma<-var(wariancje)
  sigma
}
jednorodnosc.rozlaczna(gtempdiff,10)

## formalny test stacjonarnosci ##
adf.test(gtempdiff) #p-value<0.01

##PACF
tsdisplay(gtempdiff,main="")  #ladnie
pacf(gtempdiff,main="") #rząd=3
dopasuj(3,-0.35,0.2)

#dopasowanie modelu
dopasuj<-function(p,down,up)
{
  dopasowanie.pacf<-ar(gtempdiff,method="yule-walker",aic=FALSE,order.max=p)
  wspolczynniki<-dopasowanie.pacf$ar
  wspolczynniki
  pacf.model<-ARMAacf(ar=wspolczynniki,lag.max=30,pacf=TRUE)
  pacf.prob<-pacf(gtempdiff,plot=FALSE,lag.max=30)$acf
  plot(pacf.model,type="h",col="red",lwd=3,main="pacf",ylab="",xlab="h",ylim=c(down,up))
  lines(1:30+0.1,pacf.prob,type="h",col="green",lwd=3)
  abline(h=1.96/sqrt(length(gtempdiff)), lty=2, col="red")
  abline(h=-1.96/sqrt(length(gtempdiff)), lty=2, col="red")
  legend("bottomright", legend=c("próbkowa","teoeretyczna"), col=c("green","red"),lwd=3)
  tsdisplay(dopasowanie.pacf$resid,main="")
  bp<-Box.test(dopasowanie.pacf$resid)
  lb<-Box.test(dopasowanie.pacf$resid,type="Ljung")
}

#algorytmy do AIC i Yule-Walkera sa juz zaimplementowane
dopasowanie<-ar(gtemp,method="yule-walker")
gtemp.aic<-dopasowanie$aic
plot(gtemp.aic[2:22],main="Kryterium Akaike dopasowania modelu AR",ylab="AIC",xlab="rząd",col="blue")
#dopasowanie$ar
#dopasowanie
dopasowanie$order
sigma<-var(na.omit(as.vector(dopasowanie$resid)))
dopasuj(4,-0.35,0.2)
Box.test(dopasowanie$resid)

## FPE(p) ##

fpe <- function(X, p)
{
  n <- length(X)
  dopasowanie<-ar(X,method="yule-walker",aic=FALSE,order.max=p)    #dla aic=FALSE dopasowuje model rzedu p
  sigma<-var(na.omit(as.vector(dopasowanie$resid)))
  if(p < n)
  {
    fpe<- sigma*(n+p)/(n-p)
  }
  else
  {
    fpe <- "NA"
  }
  fpe
}

## Funkcja wyznaczajaca minimalny rzad modelu AR metoda FPE ##

min.fpe <- function(X)
{
  
  n <- length(X)
  w <- c(1:20)
  fpe.p <- sapply(w, FUN = function(p){ fpe(X,p) } )
  min <- which.min(fpe.p)
  plot(fpe.p,col="blue",xlab="p",ylab="FPE(p)")
  min
}

min.fpe(gtempdiff)
dop<-ar(gtempdiff,method="yule-walker",aic=FALSE,order.max=3)
dop
#inna metoda:
yw(gtempdiff,3)

#Model dopasowany metodą najwiekszej wiarogodnosci

model1<-arima(gtempdiff,order=c(3,0,0),method="ML")
model1
model2<-arima(gtempdiff,order=c(4,0,0),method="ML")
model2 

#par(mfrow=c(1,1))
dop2<-ar(gtempdiff,method="yule-walker",aic=FALSE,order.max=4)
#pacf.yuleWalker<-ARMAacf(ar=dopasowanie$ar,lag.max=30,pacf=TRUE)
#dop2$ar
#pacf.model<-ARMAacf(ar=model2$model$phi,lag.max=30,pacf=TRUE)
#pacf.prob<-pacf(gtempdiff,plot=FALSE,lag.max=30)$acf
#plot(pacf.model,type="h",col="red",lwd=3,main="pacf",ylab="",xlab="h",ylim=c(-0.5,0.5))
#lines(1:30+0.1,pacf.prob,type="h",col="green",lwd=3)
#lines(1:30+0.2,pacf.yuleWalker,type="h",col="blue",lwd=3)
#abline(h=1.96/sqrt(length(gtempdiff)), lty=2, col="red")
#abline(h=-1.96/sqrt(length(gtempdiff)), lty=2, col="red")
#legend("bottomright", legend=c("próbkowa","teoeretyczna"), col=c("green","red"),lwd=3)

#badanie dopasowania dop AR(3), dop2 - AR(4)
war1<- diag(dop$asy.var.coef)
war2<- diag(dop2$asy.var.coef)

istotnosc<-function(nr.param,wariancje,wspolczynniki)
{
  lewy <- wspolczynniki[nr.param]-qnorm(0.9999)*sqrt(wariancje[nr.param])
  prawy<- wspolczynniki[nr.param]-qnorm(0.0001)*sqrt(wariancje[nr.param]) 
  if(lewy>0 | 0>prawy)
    cat(paste("istotny"))
  else
    cat(paste("nieistotny"))
}

istotnosc(1,war1,dop$ar)
istotnosc(2,war1,dop$ar)
istotnosc(3,war1,dop$ar)
istotnosc(1,war2,dop2$ar)
istotnosc(2,war2,dop2$ar)
istotnosc(3,war2,dop2$ar)
istotnosc(4,war2,dop2$ar)

