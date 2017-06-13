library(fBasics)
library(tseries)
library(zoo)
library(forecast)
library(lmtest)
library(fGarch)
library(rugarch)
library(tseries)
library(zoo)

#set working directory
setwd("/Users/sarahcummings/Documents/csc425")
#read in data
myd<-read.csv('google.csv')
#make zoo object because ts is hard to manipulate for weekday data
ts<-zoo(myd$Price,as.Date(as.character(myd$Date),format = c("%m/%d/%Y")))

rets = log(ts/lag(ts, -1))
retsRaw = coredata(rets)


length(df$Date)
ts1<-ts(df,start = c(2005,4), end=c(2015,209), frequency=250)
head(df)


zoo(myd$Price, as.Date(as.character(mdf$Date),
                       format=c("%m/%d/%Y")))


length(ts1)
length(rets)
coredata(rets)

logRets = log(df$Price/lag(df$Price, -1))
df2<-data.frame(df$Date,logRets)
df2

df$LR<-logRets

logrets = coredata(logRets)


