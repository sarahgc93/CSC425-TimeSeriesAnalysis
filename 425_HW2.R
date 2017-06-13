library(fBasics)
library(tseries)
library(zoo)

#set working directory
setwd("/Users/sarahcummings/Documents/csc425")
df<-read.csv('ppi_grocery.csv')
head(df)
change<-ts(df$change,start= c(2005,1),end = c(2016,8), frequency = 12)
plot(change, main= "Time plot of PPI Change")


#Compute Analysis of autocorrelations up to lag 15
myACF<-acf(coredata(change), plot=T, lag=15)
#pring the lags to consol
myACF

#Box test
Box.test(change,lag=3,type='Ljung')
Box.test(change,lag=6,type='Ljung')

myPACF<-pacf(coredata(change),plot = T`,lag=15)

library(forecast)
#make a model
m2=Arima(change, c(2,0,0), method="ML") 

library(lmtest)
coeftest(m2)
acf(m2$resid)
Box.test(m2$resid, lag=3, type='Ljung', fitdf=2)
Box.test(m2$resid, lag=6, type='Ljung', fitdf=2)
Box.test(m2$resid, lag=9, type='Ljung', fitdf=2)

#hist of residuals
hist(m2$residuals)
#normal prob plot of residuals
qqnorm(m2$residuals)
qqline(m2$residuals, col = 2)

polyroot(c(1,-m2$coef[1:2]))
plot(m2)

predict(m2,n.ahead=5, se.fit=T)
forecast.Arima(m2, h=5)
f=forecast.Arima(m2, h=10)
plot(forecast.Arima(m2, h=10), include=100)

