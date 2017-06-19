#***********************
#Google Import Function:
#***********************

#Last NA observed on each single xts instead of the entire close matrix.
#Because if there is no data for a particular stock on later dates,
#those dates wont be included in the xts.

library(lubridate)

get_googleData <- function(Tickers,from = NA,to = today()){
  
  #Add "JSE:" to the code
  Tickers = paste0("JSE:",Tickers)
  
  #IMPROVE: CHECK THAT ALL TICKERS HAVE BEEN DOWNLOADED!!!
  #IMPROVE: And display a message listing those that havent been downloaded.
  if(is.na(from)){
    getSymbols.google(Tickers,to = to,env = globalenv())  
  }else{
    getSymbols.google(Tickers,from=from,to = to,env = globalenv())
  }
  
  #Create Close Matrix:
  #Fill Close Matrix:
  close.mat = get(Tickers[1])[,4]
  if(length(Tickers) > 1){
    for(i in 2:length(Tickers)){
      temp.close = na.locf(get(Tickers[i])[,4])
      if(is.na(index(temp.close))){
        temp.close = xts(array(rep(0,nrow(close.mat)),
                               dim=c(nrow(close.mat),1),
                               dimnames = list(1:nrow(close.mat),colnames(temp.close))),
                         order.by = index(close.mat))
      }
      close.mat = merge.xts(close.mat,temp.close)
    }
  } 
  
  ##CLEAN DATA:
  #IMPOVE: Give a warning of tickers that were removed.
  #Remove Rows With All NA.
  close.mat <- close.mat[rowSums(is.na(close.mat))!= ncol(close.mat),,drop = FALSE]
  #Remove columns with all NAs
  close.mat <- close.mat[,colSums(is.na(close.mat))!= nrow(close.mat),drop = FALSE]
  #Carry Forward Last Observations:
  #close.mat <- na.locf(close.mat)
  
  #Convert to Matrix
  close.mat <- as.matrix(close.mat)
  
  #Remove Public Holidays
  
  #Change row names to standard tickers:
  colnames(close.mat) = sub("JSE.","",colnames(close.mat))
  colnames(close.mat) = sub(".Close","",colnames(close.mat))
  
  ##IMPROVE: Remove the ticker objects from the environment.
  rm(list = Tickers)
  
  return( close.mat)
}
