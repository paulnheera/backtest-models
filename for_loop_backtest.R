
#Objetcs:
#TS      (Market Data)
#fastSMA (Indicator)
#slowSMA (Indicator)
#position
#take_profit
#stop_loss
#trade
#trade_filter ???

#---- Load Libraries ----
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)

getSymbols('AGL.JO')

ts = na.locf(AGL.JO)

adj_ts = ts; adj_ts[,] = NA  #To be replaced with hit stop losses and target profits

fastSMA = SMA(Cl(ts),14)
slowSMA = SMA(Cl(ts),50)

bbands = BBands(Cl(ts),20)
bbands = cbind(Cl(ts),bbands)

majorSMA = SMA(Cl(ts),200)

trade_filter = NA

position = fastSMA; position[,] = 0
stop_loss = fastSMA; stop_loss[,] = NA
take_profit = stop_loss

exit_to_enter = FALSE  #TRUE if short exit conditions are the same as long enrty conditions and vice versa.


long_entry_cond = "(fastSMA[i] > slowSMA[i]) && (fastSMA[i-1] < slowSMA[i-1])"
short_entry_cond = "(fastSMA[i] < slowSMA[i]) && (fastSMA[i-1] > slowSMA[i-1])"

long_filter = SMA(Cl(ts),200) < SMA(Cl(ts),100)
short_filter = SMA(Cl(ts),200) > SMA(Cl(ts),100)

for_loop_backtest <- function(OHLC="",Indicator=""){
  
  #ts = OHLC
  #bbands = Indicator
  
  for(i in (n+1):nrow(ts)){
  
  #---- Check for Open:----
  if(position[i-1] == 0){ #If there was no position open:
    
    #Strategy rule: Open Long
    if((bbands[i,'dn'] > Cl(ts)[i]) && (bbands[i-1,'dn'] < Cl(ts)[i-1]) && long_filter[i]){
      position[i] = 1
      stop_loss[i] = NA
      take_profit[i] = NA  
      ##IMPROVE: Make the 3 Statements a method that can be run when closing a short position.
                        
    #Strategy rule: Open Short  
    }else if((bbands[i,'up'] < Cl(ts)[i]) && (bbands[i-1,'up'] > Cl(ts)[i-1]) && short_filter[i]){
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
      
      if((bbands[i,'mavg'] > Cl(ts)[i]) && (bbands[i-1,'mavg'] < Cl(ts)[i-1])){
        
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
      
      if((bbands[i,'mavg'] <= Cl(ts)[i]) && (bbands[i-1,'mavg'] > Cl(ts)[i-1])){
        
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
  
  ret = ROC(Cl(ts))
  
  strat_ret = ret*lag(position) 

  table.AnnualizedReturns(strat_ret)[3,]
}


#---- Optimization ----
N <- seq(5,100,by=5)

sharpe_ratios <- data.frame(look_back = N,`Sharpe Ratio`=NA,
                            check.names = FALSE)

for(n in N){
  bbands = BBands(Cl(ts),n)
  
  for_loop_backtest()
  
  sharpe_ratios[which(N == n),2] = for_loop_backtest()
  
}

#Plot Positions:
plot(position)

pos_df = data.frame(Date = index(position),Position = position,check.names = FALSE)
colnames(pos_df) =c('Date','Position')

ggplot(pos_df,aes(x=as.Date(Date),y=Position)) +
  geom_bar(stat='identity') +
  scale_x_date(labels=date_format("%b-%Y"))
  


#Performance:
ret = ROC(Cl(ts))
adj_ret = ROC(Ad(ts))

hist(ret,breaks=100)
hist(adj_ret,breaks = 100)

strat_ret = ret*lag(position) 

hist(strat_ret,breaks=100)
hist(adj_ret,breaks = 100)

charts.PerformanceSummary(strat_ret)
