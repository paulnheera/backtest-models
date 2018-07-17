
#Objetcs:
#TS      (Market Data)
#fastSMA (Indicator)
#slowSMA (Indicator)
#position
#take_profit
#stop_loss
#trade
#trade_filter ???

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)

getSymbols('AGL.JO')

ts = na.locf(AGL.JO)

adj_ts = ts; adj_ts[,] = NA  #To be replaced with hit stop losses and target profits

fastSMA = SMA(Cl(ts),14)
slowSMA = SMA(Cl(ts),50)

bbands = BBands(Cl(ts),20)

majorSMA = SMA(Cl(ts),200)

trade_filter = NA

position = fastSMA; position[,] = 0
stop_loss = fastSMA; stop_loss[,] = NA
take_profit = stop_loss

exit_to_enter = TRUE  #TRUE if short exit conditions are the same as long enrty conditions and vice versa.


for(i in 51:nrow(ts)){
  
  #---- Check for Open:----
  if(position[i-1] == 0){ #If there was no position open:
    
    #Strategy rule: Open Long
    if((fastSMA[i] > slowSMA[i]) && (fastSMA[i-1] < slowSMA[i-1])){
      position[i] = 1
      stop_loss[i] = NA
      take_profit[i] = NA  
      ##IMPROVE: Make the 3 Statements a method that can be run when closing a short position.
                        
    #Strategy rule: Open Short  
    }else if((fastSMA[i] < slowSMA[i]) && (fastSMA[i-1] > slowSMA[i-1])){
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
      }
      
    }
    
  }
  #----
}


#Plot Positions:
plot(position)

pos_df = data.frame(Date = 1:nrow(position),Position = position,check.names = FALSE)
colnames(pos_df) =c('Date','Position')

ggplot(pos_df,aes(x=Date,y=Position)) +
  geom_bar(stat='identity')
#fill = ifelse(pos_df$Position==1,'green','red')


#Performance:
ret = ROC(Cl(ts))
adj_ret = ROC(Ad(ts))

hist(ret,breaks=100)
hist(adj_ret,breaks = 100)

strat_ret = ret*lag(position) 

hist(strat_ret,breaks=100)
hist(adj_ret,breaks = 100)

charts.PerformanceSummary(strat_ret)
