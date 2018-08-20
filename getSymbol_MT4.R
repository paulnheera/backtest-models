#Get Symbol historical Data from MT4 csv file

library(readr)
library(quantmod)

getSymbol.MT4 <- function(symbol,timeframe){
  

  file = paste0("MT4 Data/",symbol,timeframe,".csv")
  
  temp = read_csv(file,
                  col_names = c('Date','Time','Open','High','Low','Close','Volume'))
  
  temp = as.xts(temp[,c('Open','High','Low','Close','Volume')], 
                order.by= as.POSIXct(paste(temp$Date,temp$Time),format="%Y.%m.%d %H:%M", tz='GMT'))
  
  return(temp)
}

GBPUSD15M = getSymbol.MT4("GBPUSD",15)

BTCUSD = getSymbol.MT4("MT4 Data/Dump/BTCUSD1440.csv")
BTCEUR = getSymbol.MT4("MT4 Data/Dump/BTCEUR1440.csv")
EURUSD = getSymbol.MT4("MT4 Data/Dump/EURUSD1440.csv")

saveRDS(AUDUSD,"MT4 Data/Database/AUDUSD1H.rds")
saveRDS(USDCAD,"MT4 Data/Database/USDCAD1H.rds")




