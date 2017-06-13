library(fBasics)
library(tseries)
library(zoo)
library(forecast)
library(lmtest)


#set working directory
setwd("/Users/sarahcummings/Documents/csc425")
df<-read.csv('banana_price.csv')
head(df)


#create two timeseries objects
priceTS<-ts(df$bananas,start = c(2010,1),end = c(2016,8), frequency = 12)
changeTS<-ts(df$change,start = c(2010,1),end = c(2016,8), frequency = 12)

#timeplot of Price TS
plot(priceTS,ylab='Banana Price per metric ton (USD)', main= "Time plot of Banana Price")
#distibution of price histogram
hist(df$bananas,xlab = 'banana price (USD)', main= "histogram of banana prices")

basicStats(df$bananas)
qqnorm(df$bananas, main = 'Normal QQ plot for Banana Price')
qqline(df$bananas, col = 2)

#analysis of stationary behavior
acf(df$bananas)
pacf(df$bananas)

#Box test
Box.test(priceTS,lag=3,type='Ljung')
Box.test(priceTS,lag=6,type='Ljung')

# apply a automated order selection procedure
auto.arima(priceTS, stationary=T, seasonal=T)


#Fit an MA model
m1= Arima(priceTS, order=c(0,0,2), method='ML', include.mean=T)
m1
# T-tests on coefficients
coeftest(m1)

# RESIDUAL ANALYSIS
Box.test(m1$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m1$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m1$residuals,lag=12,type='Ljung', fitdf=2)
acf(m1$residuals)

hist(m1$residuals)
qqnorm(m1$residuals)
qqline(m1$residuals)

acf(m1$resid, main="ACF of residuals")

#Fit an AR model
m2=Arima(priceTS, order=c(2,0,0))
m2
#Coeff test
coeftest(m2)

# RESIDUAL ANALYSIS
Box.test(m2$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m2$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m2$residuals,lag=12,type='Ljung', fitdf=2)
acf(m2$residuals)

hist(m2$residuals)
qqnorm(m2$residuals)
qqline(m2$residuals)

#Forecast
forecast.Arima(m1, h=10)
plot(forecast.Arima(m1, h=20))

forecast.Arima(m2, h=10)
plot(forecast.Arima(m2, h=20))

auto.arima(priceTS, max.P=8, max.Q=8, ic="bic")

#Backtesting
ntest=round(length(priceTS)*0.8)
source("backtest.R")
pm2 = backtest(m1, priceTS, ntest, 1)
pm3 = backtest(m2, priceTS, ntest, 1)

#Plot for change ts
plot(changeTS,ylab='Change in Banana Price(USD)', main= "Time plot of Change in Banana Price")

#Analysis of ACF and PACF for change
acf(df$change)
pacf(df$change)

m3<-Arima(changeTS, order=c(2,0,0), include.mean=F)
m3
coeftest(m3)

# RESIDUAL ANALYSIS
Box.test(m3$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m3$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m3$residuals,lag=12,type='Ljung', fitdf=2)
acf(m3$residuals)

hist(m3$residuals)
qqnorm(m3$residuals)
qqline(m3$residuals)
