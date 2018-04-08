#*****************
# Momentum Report:
#*****************

#---- Load Libraries ----
library(quantmod)
library(PerformanceAnalytics)
library(rvest)
library(readxl)
library(readr)
library(dygraphs)
library(lubridate)
library(ggplot2)
library(reshape2)

source("momentum_strat.R")
source("portfolio_backtest.R")

#---- Load Data ----
jalsh_adjusted <- readRDS("~/repos/google-finance-database/jalsh_adjusted.rds")

getSymbols("STX40.JO",env = globalenv())
stx40 = Ad(STX40.JO)
colnames(stx40) = "STX40"
#---- Run Backtest ----

Port_Wght = MoM_strat(jalsh_adjusted,9,3,3,startDate = "2008-01-01")

#First Quantile portfolio:
Port_Out = runBacktest(Port_Wght[,,1],jalsh_adjusted,100000)

#---- Monthly Performance Evaluation ----

Port_NAVs = Port_Out[,"Total",drop=F]
Port_NAVs = xts(Port_NAVs,order.by = as.Date(rownames(Port_NAVs)))


#Merge with Benchmark STX40:
navs = merge.xts(Port_NAVs,stx40)

navs[,2] = na.locf(navs[,2])

navs = navs[!is.na(navs[,1]),]


##IMPROVE:When collecting Month end points remember to include 
##start value as the month end of the previous month to start date

x = navs[c(1,endpoints(navs)),,drop=F]
#index(x)[1] = index(x)[1]-1


ret = ROC(x)

charts.PerformanceSummary(ret)

#---- Summary Table ---- 

summary = data.frame(Fund="Momentum",MTD = NA,`1 Month` = NA, `3 Months` = NA, `6 Months` = NA, `12 Months` = NA,
                     `3 Years` = NA, Inception= NA,`Volatility` = NA,
                     check.names = FALSE)

summary$MTD = last(ret,1)
summary$`3 Months` = Return.cumulative(last(ret,3))
summary$`6 Months` = Return.cumulative(last(ret,6))
summary$`12 Months` = Return.cumulative(last(ret,12))
summary$`3 Years` = Return.annualized(last(ret,6))
summary$Inception = Return.annualized(ret,scale=12)
summary$Volatility = StdDev.annualized(ret,scale=12)

yearly = table.CalendarReturns(ret)[,c(13,14)]
yearly = data.frame(Year=rownames(yearly),yearly)

yearly.long = melt(yearly,id.vars="Year")

ggplot(yearly.long, aes(x=Year,y=value,fill=variable)) +
  geom_bar(position = "dodge",stat="identity") +
  theme_bw()








