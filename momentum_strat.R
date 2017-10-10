# ************************
# Momentum Strategy  *
# ************************

#**********************************************************************************

#******************************
#Portfolio Selection (Weights)
#******************************
##Input Data:
# in.close is Historical Share prices. Matrix with rownames as dates.
# mkt.cap is the Market Capitalization. Matrix with rownames as dates.
##Parameters:
# limit is the limit to the number of shares in the universe according to Market Cap.
# J is the formation period.
# H is the holding period.
# R is the rebalncing frequency. (In Months)
# StartDate is the Date on which the backtest starts.

library(quantmod)
source("~/repos/backtest_models/google_import.R")

MoM_strat <- function(in.close,J,H,R,startDate,weights = "Equal Weights"){
  
  #Remove Public Holidays:
  PH = (which(rowSums(ROC(in.close),na.rm = TRUE) == 0))[-1]
  if(length(PH)){
    in.close = in.close[-PH,]
  }
  
  #Row Names and Column Names:
  ts.Dates = row.names(in.close)
  tickers = colnames(in.close)
  
  #Periodicity:
  if(periodicity(in.close)$scale=="monthly"){
    j = J
    h = H
    r = R
  }else if(periodicity(in.close)$scale=="daily"){
    j = 21*J    # Formation Period.
    h = 21*H    # Holding Period.
    r = 21*R    # Rebalancing frequency. Should be the same as J by default.
  }
  

  #Start Date:
  ts.Dates = as.Date(ts.Dates)
  startPos = min(which(ts.Dates >= startDate))
  
  #Momentum:
  MoM.mat <- ROC(in.close,n = j)
  
  #Make Start date the latest date with Momentum Figures:
  if(startDate < ts.Dates[j]){
    startPos=min(which(rowSums(MoM.mat,na.rm = T) != 0))
    startDate = ts.Dates[startPos]
  }
    
  #Risk - standard deviation:
  #ret = ROC(in.close)
  #realized.vol = array(NA,dim = dim(ret))
  #for(i in 1:ncol(ret)){
  #   realized.vol[,i] = tryCatch(runSD(ret[,i],n=20),error = function(e) NA)
  #}
  #realized.vol = as.matrix(apply(realized.vol,2,as.numeric))
  #colnames(realized.vol) = colnames(in.close)
  #rownames(realized.vol) = row.names(in.close)
  #How many Quantiles
  
  #Portfolios:
  Portfolios = array(data = NA,dim = c(nrow(in.close),ncol(in.close),5), 
                     dimnames = list(row.names(in.close),colnames(in.close)))
  
  array1 = seq(startPos,nrow(in.close),by = h) #Portfolio Turnover Points.
 
  for(i in seq(startPos,nrow(in.close),by = r))
  {
    if(is.element(i,array1))
    {
      
      D = quantile(MoM.mat[i,],probs = seq(0,1,0.20), na.rm = TRUE)
      
      #Anomalies at each bar.
      anom.pos = which(MoM.mat[i,] > 1)
      stocks = colnames(MoM.mat)[anom.pos]
      warning(paste0("The following stocks have abnormal returns at bar ",
                     i,"\n",stocks))
      
      pos1 = which(MoM.mat[i,] > D[5])
      pos2 = which(MoM.mat[i,] > D[4] & MoM.mat[i,] <= D[5])
      pos3 = which(MoM.mat[i,] > D[3] & MoM.mat[i,] <= D[4])
      pos4 = which(MoM.mat[i,] > D[2] & MoM.mat[i,] <= D[3])
      pos5 = which(MoM.mat[i,] <= D[2])
    }
    
    if(weights == "Equal Weights"){
      # Equal Weighted!!!!
      Portfolios[i,pos1,1] = 1                      # First Quantile Portfolio
      Portfolios[i,which(is.na(Portfolios[i,,1]) == "TRUE"),1] = 0
      Portfolios[i,,1] = Portfolios[i,,1]/sum(Portfolios[i,,1])     # Equal Weighted 
      
      Portfolios[i,pos2,2] = 1                      # Second Quantile Portfolio
      Portfolios[i,which(is.na(Portfolios[i,,2]) == "TRUE"),2] = 0  
      Portfolios[i,,2] = Portfolios[i,,2]/sum(Portfolios[i,,2])     # Equal Weighted
      
      Portfolios[i,pos3,3] = 1                      # Third Quantile Portfolio
      Portfolios[i,which(is.na(Portfolios[i,,3]) == "TRUE"),3] = 0
      Portfolios[i,,3] = Portfolios[i,,3]/sum(Portfolios[i,,3])     # Equal Weighted
      
      Portfolios[i,pos4,4] = 1                      # Fourth Quantile Portfolio
      Portfolios[i,which(is.na(Portfolios[i,,4]) == "TRUE"),4] = 0
      Portfolios[i,,4] = Portfolios[i,,4]/sum(Portfolios[i,,4])     # Equal Weighted
      
      Portfolios[i,pos5,5] = 1                      # Fifth Quantile Portfolio
      Portfolios[i,which(is.na(Portfolios[i,,5]) == "TRUE"),5] = 0
      Portfolios[i,,5] = Portfolios[i,,5]/sum(Portfolios[i,,5])     # Equal Weighted
    }
    
    #Inverse Volatility Weighted:
    if(weights == "Inverse Risk"){
      #First Quantile Portfolio:
      Portfolios[i,pos1,1] = 1                      
      Portfolios[i,which(is.na(Portfolios[i,,1]) == "TRUE"),1] = 0
      Portfolios[i,pos1,1] = Portfolios[i,pos1,1]/realized.vol[i,pos1]     
      Portfolios[i,,1] = Portfolios[i,,1]/sum(Portfolios[i,,1])
      
      #Second Quantile Portfolio:
      Portfolios[i,pos2,2] = 1                      
      Portfolios[i,which(is.na(Portfolios[i,,2]) == "TRUE"),2] = 0  
      Portfolios[i,pos2,2] = Portfolios[i,pos2,2]/realized.vol[i,pos2]    
      Portfolios[i,,2] = Portfolios[i,,2]/sum(Portfolios[i,,2])
      
      #Third Quantile Portfolio:
      Portfolios[i,pos3,3] = 1                      
      Portfolios[i,which(is.na(Portfolios[i,,3]) == "TRUE"),3] = 0
      Portfolios[i,pos3,3] = Portfolios[i,pos3,3]/realized.vol[i,pos3]     
      Portfolios[i,,3] = Portfolios[i,,3]/sum(Portfolios[i,,3])
      
      #Fourth Quantile Portfolio:
      Portfolios[i,pos4,4] = 1                      
      Portfolios[i,which(is.na(Portfolios[i,,4]) == "TRUE"),4] = 0
      Portfolios[i,pos4,4] = Portfolios[i,pos4,4]/realized.vol[i,pos4]     
      Portfolios[i,,4] = Portfolios[i,,4]/sum(Portfolios[i,,4])
      
      #Fifth Quantile Portfolio:
      Portfolios[i,pos5,5] = 1                      
      Portfolios[i,which(is.na(Portfolios[i,,5]) == "TRUE"),5] = 0
      Portfolios[i,pos5,5] = Portfolios[i,pos5,5]/realized.vol[i,pos5]     
      Portfolios[i,,5] = Portfolios[i,,5]/sum(Portfolios[i,,5])
    }
  }
  period = startPos:nrow(in.close)
  Portfolios = Portfolios[period,,]
  return(Portfolios)
}

##Notes:
# Make sure in.close matrix is cleaned. 
#portWeights = MoM_strat(in.close,3,3,"2006-01-01")
#PortWghts = MoM_strat_v3(in.close,mkt.cap,100,3,3,3,"2006-01-01")
