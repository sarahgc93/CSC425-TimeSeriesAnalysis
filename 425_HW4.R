library(fBasics)
library(tseries)
library(zoo)
library(forecast)
library(lmtest)


#set working directory
setwd("/Users/sarahcummings/Documents/csc425")

df<-read.csv('consump2.csv')
head(df)
length(df$Energy)
df[522,]
ts1<-ts(df$Energy,start = c(1973,1),end=c(2016,6),frequency = 12)

#distribution analysis
basicStats(df$Energy)
qqnorm(df$Energy, main = 'Normal QQ plot for Energy Consumption')
qqline(df$Energy, col = 2)

energy<-df$Energy

#Jarque-Bera normality test
normalTest(energy,method=c("jb"))
hist(df$Energy, main = 'Histogram of Energy Consumption', xlab='Energy Consumption (Trillion BTU)')

#make a time plot
plot(ts1,main="Timeplot of Energy Consumption", ylab="Energy Consumption (Trillion BTU)")
plot

#apply first differencing to data
dx=diff(ts1)
acf(as.vector(dx),lag.max=26, main="ACF of DX starts")

# compute seasonal difference for data (s=12)
sdx=diff(dx,12)
#check stationarity
acf(as.vector(sdx),lag.max=30, main="ACF of DSDX starts")
#Box test
Box.test(sdx,lag=3,type='Ljung')
Box.test(sdx,lag=6,type='Ljung')

#initial SARIMA
m=auto.arima(ts1, stationary=T, seasonal=T)
coeftest(m)
m

#alternative models
m2=Arima(ts1,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12), method="ML")
coeftest(m2)
m3=Arima(ts1, order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), method="ML")
coeftest(m3)
m4=Arima(ts1, order=c(1,1,2),seasonal=list(order=c(0,1,1),period=12), method="ML")
coeftest(m4)

m5=Arima(ts1, order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), method="ML")
coeftest(m5)

# RESIDUAL ANALYSIS
Box.test(m4$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m4$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m4$residuals,lag=12,type='Ljung', fitdf=2)

Box.test(m3$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m3$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m3$residuals,lag=12,type='Ljung', fitdf=2)

Box.test(m2$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m2$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m2$residuals,lag=12,type='Ljung', fitdf=2)

Box.test(m$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m$residuals,lag=12,type='Ljung', fitdf=2)

Box.test(m5$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m5$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m5$residuals,lag=12,type='Ljung', fitdf=2)

#further residual analysis with only model that passed box test of residulals
hist(m2$residuals)
qqnorm(m2$residuals)
qqline(m2$residuals)
acf(m2$resid, main="ACF of residuals")


# compute predictions for 5 steps ahead
f1=forecast.Arima(m2,h=5)

#backtesting
source("backtest.R")
ntest=round(length(ts1)*0.8)
backtest = backtest(m2, ts1, ntest, 1)



######### PROBLEM @


df2<-read.csv('homesales.csv')
head(df2)
length(df2$DATE)
df2[200,]

ts2<-ts(df2$VALUE,start = c(2000,1),end = c(2016,8),frequency = 12)
plot(ts2,main='Timeplot of Median Sale Prices of US houses')

acf(df2$VALUE)
pacf(df2$VALUE)

#Dickey Fuller test
# Load library
library(fUnitRoots)
adfTest(ts2, lags=3,type=c("c"))
adfTest(ts2,lags=5,type=c("c"))
adfTest(ts2,lags=7,type=c("c"))

m=auto.arima(ts2, stationary=F, seasonal=F)
coeftest(m)

Box.test(m$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m$residuals,lag=12,type='Ljung', fitdf=2)
#this model didnt pass the residual analysis so I'm taking the log of the data and trying again


hist(m$residuals)
qqnorm(m$residuals)
qqline(m$residuals)


df2$LnValue<-log(df2$VALUE)
head(df2)
ts3<-ts(df2$LnValue,start = c(2000,1),end = c(2016,8),frequency = 12)
plot(ts3)

m6=auto.arima(ts2, stationary=F, seasonal=F)
coeftest(m6)
Box.test(m6$residuals,lag=6,type='Ljung', fitdf=2)
Box.test(m6$residuals,lag=10,type='Ljung', fitdf=2)
Box.test(m6$residuals,lag=12,type='Ljung', fitdf=2)

# compute predictions for 5 steps ahead
f2=forecast.Arima(m,h=5)
f2

#backtesting
source("backtest.R")
ntest=round(length(ts2)*0.8)
backtest = backtest(m2, ts1, ntest, 1)


