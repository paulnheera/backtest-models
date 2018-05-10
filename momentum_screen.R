#*****************************
# Momentum Screening Function
#*****************************

#Description:
# Takes xts time series and returns a list of the quantiles (data.frames)
# Takes formation period.

momentum_screen <- function(x,J){
  
  x = na.locf(x) # Make sure NAs are dealt with.
  
  ret = ROC(x,J*21,type = "discrete")
  
  Q = quantile(xts::last(ret),na.rm=TRUE)
  
  Q1 = xts::last(ret)[,which(xts::last(ret) > Q[4])]
  
  Q1 = data.frame(Stock = colnames(Q1), Momentum = t(Q1))
  colnames(Q1) = c("Stock","Momentum")
  Q1 = Q1 %>% 
    arrange(desc(Momentum))
  
  return(Q1)
  
}