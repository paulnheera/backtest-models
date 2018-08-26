#**********************
# Candlestick Patterns
#**********************

#---- Load Libraries ----
#IMPROVE: Load Libraries once in the terminals active for MQL4 .
#IMPROVE: So load libraries on Init.
library(quantmod)
library(dplyr)
library(candlesticks)
library(dygraphs)
library(mailR)
source("~/repos/Drafts/TrendDetectionChannel2.R")

#---- Data Wrangling ----

#IMPROVE: Receive pivot points
df = data.frame(Time=Time,Open=Open,High=High,Low=Low,Close=Close)

ts = xts(df[,-1],order.by = as.POSIXct(df$Time,format="%Y.%m.%d %H:%M", tz='GMT'))

#---- Parameters ----
Action =FALSE
position = 0
stop_loss = 0
take_profit = 0
lot_size = as.double(0)

#---- Check for Hammer Downtrend Reversal ----
Hammer = CSPHammer(ts) & TrendDetectionChannel2(ts,n=14)[,"DownTrend"]

if(last(Hammer)){# There is a hammer:
  
  position = 1                #Buy/Long
  stop_loss = last(Lo(ts))
  take_profit = 0
  lot_size = as.double(0.01)
  
}

#---- Check for Inverted Hammer Uptrend Reversal ----
InvertedHammer = CSPInvertedHammer(ts) & TrendDetectionChannel2(ts,n=14)[,"UpTrend"]

if(last(InvertedHammer)){
  
  position = -1                #Sell/Short
  stop_loss = last(Hi(ts))
  take_profit = 0
  lot_size = as.double(0.01)
  
}

#---- Check for Morning Star Downtrend Reversal ----
MorningStar = CSPStar(ts)[,"MorningStar"] & TrendDetectionChannel2(ts,n=14)[,"DownTrend"]

if(last(MorningStar)){# There is a Morning Star:
  
  position = 1                #Buy/Long
  stop_loss = min(xts::last(Lo(ts),3))
  take_profit = 0
  lot_size = as.double(0.01)
  
}

#---- Check for Evening Star Uptrend Reversal ----
EveningStar  = CSPStar(ts)[,"EveningStar"] & TrendDetectionChannel2(ts,n=14)[,"UpTrend"]

if(last(EveningStar)){
  
  position = -1                #Sell/Short
  stop_loss = max(xts::last(Hi(ts),3))
  take_profit = 0
  lot_size = as.double(0.01)
  
}

#---- Check for Piercing Pattern Downtrend Reversal ----
#Piercing Line
PiercingLine = CSPPiercingPattern(ts) & TrendDetectionChannel2(ts,n=14)[,"DownTrend"]

#---- Check for Dark Cloud Cover Pattern Uptrend Reversal ---- 
DarkCloudCover = CSPDarkCloudCover(ts) & TrendDetectionChannel2(ts,n=14)[,"UpTrend"]

#---- Check for Doji Trend Reversal ----
# Next candle should be in the direction of the reversal.

#Uptrend Reversal:
Doji_bear = CSPDoji(ts)[,"Doji"] & TrendDetectionChannel2(ts,n=14)[,"UpTrend"]

#Downtrend Revesal:
Doji_bull = CSPDoji(ts)[,"Doji"] & TrendDetectionChannel2(ts,n=14)[,"DownTrend"]


#---- Was there any action ----

if(last(Hammer)){
  
  pattern = "Hammer"
  Action = TRUE
  
}else if(last(InvertedHammer)){
  
  pattern = "InvertedHammer"
  Action = TRUE
}else if(last(EveningStar)){
 
  pattern = "EveningStar"
  Action = TRUE 
  
}else{
  pattern = "No Pattern"
}

#---- Check proximity to pivot Point ----

#---- Save Data ----
##IMPROVE: Save with time-frame in name.

file_name = paste0(Symbol," ",xts::last(index(ts)),".rds")
file_name = gsub(":",".",file_name)

saveRDS(ts,paste0('~/repos/Data/',file_name))

#---- Send Output to MetaTrader ----

#---- Email Alert ----
if(Action){
  
  body = paste0("We have a ",pattern," on the ",Symbol, " Chart. ",
                "\nTime: ", xts::last(index(ts)),
                "\nPrice: ",last(Cl(ts)),
                "\nLook to Trade!!")
  
  send.mail("paul.nheera@gmail.com","paul.nheera@gmail.com",subject = "Test",
            body = body,
            smtp = list(host.name="smtp.gmail.com",port=465,
                        user.name = "paul.nheera@gmail.com", passwd = "Mhofuu27",
                        ssl=TRUE),
            authenticate=TRUE,
            send=TRUE)
  
}

# dygraph(ts["2018-06-20 14:30:00::2018-06-20 20:30:00"]) %>% 
#   dyCandlestick() %>% 
#   dyOptions(digitsAfterDecimal=5)
  
