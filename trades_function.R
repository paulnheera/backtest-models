#******************
# Identify Trades :
#******************

get_trades <- function(position){
  
  temp <- (position != lag(position))
  temp <- index(position)[which(temp==TRUE)]
  
  start <- temp[seq(from = 1,length(temp),by=2)]
  end <- temp[seq(from = 2,length(temp),by=2)]
  
  if(length(start) > length(end)) end = c(end,NA)
  
  trades <- data.frame(start=start,end=end,position = position[start])
  
  return(trades)
}


