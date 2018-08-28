
#Objetcs:
#TS      (Market Data)
#fastSMA (Indicator)
#slowSMA (Indicator)
#position (Trade Info)
#take_profit (Trade Info)
#stop_loss (Trade Info)
#trades (Trade Info)
#trade_filter (Indicator)

#---- Load Libraries ----
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
library(readr)
library(candlesticks)
source('getSymbol_MT4.R')
source('trades_function.R')
source("~/repos/Drafts/TrendDetectionChannel2.R")


#---- Market Data ----

ts <- getSymbol.MT4('EURUSD',15)


#---- Indicators ----

#Simple Moving Average:
fastSMA = SMA(Cl(ts),5)
slowSMA = SMA(Cl(ts),14)

#Bollinger Bands:
bbands = BBands(Cl(ts),12,sd=2)
bbands = cbind(Cl(ts),bbands)

#Relative Strength Index:
rsi <- RSI(Cl(ts))

#14 bar High/Low:
n_high <- lag(runMax(Cl(ts),14))
n_low <- lag(runMin(Cl(ts),14))

#Candlesick patterns:
MorningStar = CSPStar(ts)[,"MorningStar"] & TrendDetectionChannel2(ts,n=14)[,"DownTrend"]
EveningStar  = CSPStar(ts)[,"EveningStar"] & TrendDetectionChannel2(ts,n=14)[,"UpTrend"]

#---- Inittialize Trade info Objects ----
position = Cl(ts); position[,] = 0 ;colnames(position) = "position"

#Take Profit and Stop Loss:
stop_loss = position; stop_loss[,] = NA
take_profit = stop_loss

exit_to_enter = FALSE  #TRUE if short exit conditions are the same as long enrty conditions and vice versa.

#---- Set Filters ----
long_filter =  Cl(ts); long_filter[,] = TRUE 
short_filter =  Cl(ts); short_filter[,] = TRUE

long_filter <- SMA(Cl(ts),100) < SMA(Cl(ts),50)
short_filter <- SMA(Cl(ts),100) > SMA(Cl(ts),50)

n = 100
#---- Backtest ----

#for_loop_backtest <- function(OHLC="",Indicator=""){

for(i in (n+1):nrow(ts)){
  
  #---- Check for Open:----
  if(position[i-1] == 0){ #If there was no position open:
    
    #Strategy rule: Open Long!
    if((fastSMA[i]>slowSMA[i]) && (fastSMA[i-1] < slowSMA[i-1]) && long_filter[i]){
      position[i] = 1
      stop_loss[i] = NA
      take_profit[i] = NA  
      ##IMPROVE: Make the 3 Statements a method that can be run when closing a short position.
      ##This would be for the case where conditions for opening short are the same as closing long.
      
      #Strategy rule: Open Short! 
    }else if((fastSMA[i] < slowSMA[i]) && (fastSMA[i-1] > slowSMA[i-1]) && short_filter[i]){
      ##IMPROVE: (TEST SENSITIVITY) fastSMA[i] < slowSMA[i] && lag(fastSMA,1)[i] > lag(fastSMA,1)[i]
      ##REASONING: This allows flexibility in the amount of variables checked in conditions.
      ## i.e if condition is based on the previous n bars.
      position[i] = -1
      stop_loss[i] = NA
      take_profit[i] = NA
      
      #Do nothing:
    }else{
      position[i] = position[i-1]
    }
    
  }
  #----
  
  #---- Check for close long position: ----
  if(position[i-1] == 1){ 
    
    #Check for stop loss or target profit
    if(!is.na(stop_loss[i]) || !is.na(take_profit[i])){
      
      if(Lo(TS)[i] < stop_loss[i]){
        position[i] = 0 
        trade[i] = stop_loss[i]
      }
      
      if(Hi(TS)[i] > take_profit[i]){
        position[i] = 0
        trade[i] = take_profit[i]
      }
      
    }else { #Check for Exit Rule:
      
      if((fastSMA[i] < slowSMA[i]) && (fastSMA[i-1] > slowSMA[i-1])){
        
        if(exit_to_enter){
          #Enter Short Position:
          position[i] = -1
          stop_loss[i] = NA
          take_profit[i] = NA
        }else{
          position[i] = 0
        }
        ##IMPROVE: If Exit rule is the same as entry rule for opposite direction, alter accordingly.
        
      }else{
        position[i] = position[i-1]
        stop_loss[i] = stop_loss[i-1]
        take_profit[i] = take_profit[i-1]
      }
      
    }
    
  }
  #----
  
  #---- Check for close short position: ----
  if(position[i-1] == -1){ 
    
    #Check for stop loss or target profit:
    if(!is.na(stop_loss[i]) || !is.na(take_profit[i])){
      
      if(Hi(TS)[i] > stop_loss[i]){
        position[i] = 0 
        trade[i] = stop_loss[i]
      }
      
      if(Lo(TS)[i] < take_profit[i]){
        position[i] = 0
        trade[i] = take_profit[i]
      }
      
    }else{ #Check for Exit Rule:
      
      if((fastSMA[i] > slowSMA[i]) && (fastSMA[i-1] < slowSMA[i-1])){
        
        if(exit_to_enter){
          #Enter Long Position:
          position[i] = 1
          stop_loss[i] = NA
          take_profit[i] = NA
        }else{
          position[i] = 0
        }
        ##IMPROVE: If Exit rule is the same as entry rule for opposite direction, alter accordingly.
      }else{
        position[i] = position[i-1]
        stop_loss[i] = stop_loss[i-1]
        take_profit[i] = take_profit[i-1]
      }
      
    }
    
  }
  #----
  
}

#Return Stream:
ret = ROC(Cl(ts))

#Strategy Returns:
strat_ret = ret*lag(position) 

#Table of summary stats:
table.AnnualizedReturns(strat_ret)[3,]

#Return: Positions & Strategy Returns

#}


#---- Trades ----
trades <- get_trades(position)

trades2 <- trades %>% 
  mutate(PnL = NA)

for(j in 1:nrow(trades2)){
  trades2$PnL[j] <- Return.cumulative(strat_ret[paste0(trades2$start[j],'::',trades2$end[j])])
}

#---- Visualization: Positions ----

#Subset Period:
from = "2018-05-01"
to = "2018-12-31"

start_ind <-  min(which(trades$start > from))
end_ind <- max(which(trades$start < to))

#Arrange Data:
g_ts <- ts[paste0(from,"::",to)]
gdata <- data.frame(Time = as.POSIXct(index(g_ts)),g_ts)

#Plot Graph:
ggplot() +
  geom_rect(data = trades[start_ind:end_ind,], aes(xmin = start, xmax = end, 
                                                   ymin = -Inf, ymax = Inf),
            fill = ifelse(trades[start_ind:end_ind,'position'] < 0,'red','green'),
            alpha = 0.2) +
  geom_line(data=gdata,aes(x=Time,y=Close),size=0.8,colour='blue')

#---- Visualization: With Indicators ----

#---- Visualization: Performance ----
strat_cumret <- cumprod(1+na.trim(strat_ret)) - 1

drawdowns <- Drawdowns(strat_ret)

gdata <- data.frame(Time = index(strat_cumret),strat_cumret)

ggplot(gdata,aes(x=Time,y=Close)) +
  geom_line(colour='blue',size=0.8) +
  scale_x_datetime(date_breaks = "5 years",date_labels = '%b \n %Y') +
  ylab('Cumulative Return') +
  xlab('Date')

#---- Optimization ----



