
L = seq(10,50,by=5)

for_loop_backtest <- function(lookback = 12){

  #---- Indicators ----
  
  #Simple Moving Average:
  #fastSMA = SMA(Cl(ts),14)
  #slowSMA = SMA(Cl(ts),50)
  
  #Bollinger Bands:
  bbands = BBands(Cl(ts),lookback,sd=2)
  bbands = cbind(Cl(ts),bbands)
  
  
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
  
  
  
  for(i in (n+1):nrow(ts)){
    
    #---- Check for Open:----
    if(position[i-1] == 0){ #If there was no position open:
      
      #Strategy rule: Open Long!
      if((bbands[i,'dn'] > Cl(ts)[i]) && (bbands[i-1,'dn'] < Cl(ts)[i-1]) && long_filter[i]){
        position[i] = 1
        stop_loss[i] = NA
        take_profit[i] = NA  
        ##IMPROVE: Make the 3 Statements a method that can be run when closing a short position.
        ##This would be for the case where conditions for opening short are the same as closing long.
        
        #Strategy rule: Open Short! 
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
  
  #Return Stream:
  ret = ROC(Cl(ts))
  
  #Strategy Returns:
  strat_ret = ret*lag(position) 
  
  #Table of summary stats:
  return(table.AnnualizedReturns(strat_ret)[3,])
  
  #Return: Positions & Strategy Returns

}


#---- Optimization ----
Opt <- data.frame(L,CAGR = NA)

for(i  in 1:length(L)){
  
  temp = for_loop_backtest(lookback = L[i])
  
  Opt[which(Opt[,1] == L[i] ),'CAGR'] = temp
  
  
}


ggplot(Opt,aes(x=L,y=CAGR)) +
  geom_line(colour = 'blue',size=0.8)

