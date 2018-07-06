#Convert List of Time Series to one time series Matrix:
library(quantmod)


get_2DTimeseries = function(L = "List of time series"){
  
  n = length(L)
  
  P = L[[1]] 
  P = as.xts(P[,2],order.by=P[,1])
  
  for(i in 2:n){
    temp = L[[i]]
    temp = as.xts(temp[,2],order.by=temp[,1])
    P = merge.xts(P,temp)
  }
  
  colnames(P) = names(L)
  
  return(P)
}
