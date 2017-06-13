library(fBasics)
library(tseries)
library(zoo)

#set working directory
setwd("/Users/sarahcummings/Documents/csc425")

####################
##  PROBLEM 2

#Save our data to a varaible
#Compute the basic statistics for our data
cpiData= read.csv('cpi_2001_2013.csv')
basicStats(cpiData$inflation)

#Create a histogram
hist(cpiData$inflation, xlab="Inflation", prob=TRUE, main="CPI Histogram")
# add approximating normal density curve 
xfit<-seq(min(cpiData$inflation),max(cpiData$inflation),length=40) 
yfit<-dnorm(xfit,mean=mean(cpiData$inflation),sd=sd(cpiData$inflation))
lines(xfit, yfit, col="blue", lwd=2)

#Create a normal prob plot
qqnorm(cpiData$inflation)
qqline(cpiData$inflation, col = 2)

inf<-cpiData$inflation

#Skewness test
skew_test = skewness(inf)/sqrt(6/length(inf))
skew_test
#p value
2* (1-pnorm(abs(skew_test)))

# Fat-tail test
k_stat = kurtosis(inf)/sqrt(24/length(inf))
k_stat
#p value
2*(1-pnorm(abs(k_stat)))

#Jarque-Bera normality test
normalTest(inf,method=c("jb"))


#Create a timeseries plot of CPI
#pull inflation column from data and make a timeseries object.

infTS2<- ts(data = cpiData[3], start = c(2001,1), frequency = 12)

plot(infTS2, main='Time Plot for CPI Inflation')

#Compute Analysis of autocorrelations up to lag 15
acf1<-acf(coredata(infTS), plot=T, lag=15)
#print lags to the console
acf1

#Box test
Box.test(infTS,lag=3,type='Ljung')
Box.test(infTS,lag=6,type='Ljung')


##############################
##  PROBLEM 3

walData= read.csv('walmart_sales.csv')
head(walData)

# Make a timeseries object and a time plot
salesTS<-ts(data=walData[2], start = c(2010,5), frequency = 52)
plot(salesTS, main="Walmart Sales Data")

#Create a histogram
hist(walData$Weekly_Sales, xlab="Sales Data", prob=TRUE, main="Walmart Sales Histogram")
# add approximating normal density curve 
xfit<-seq(min(walData$Weekly_Sales),max(walData$Weekly_Sales),length=40) 
yfit<-dnorm(xfit,mean=mean(walData$Weekly_Sales),sd=sd(walData$Weekly_Sales))
lines(xfit, yfit, col="blue", lwd=2)

#Create a normal prob plot
qqnorm(walData$Weekly_Sales)
qqline(walData$Weekly_Sales, col = 2)

#Compute five number summary
basicStats(walData$Weekly_Sales)
head(walData)

#Compute Analysis of autocorrelations up to lag 15
myACF<-acf(coredata(salesTS), plot=T, lag=15)
#pring the lags to consol
myACF

#Box test
Box.test(salesTS,lag=3,type='Ljung')
Box.test(salesTS,lag=6,type='Ljung')


