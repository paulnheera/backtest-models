#*************************
# Portfolio Backtest 
#*************************

#Challenges:
#Share Slpits.
#Bid/Ask Spreads.

#in.wght = Portfolios[,,1]
#in.close = PX_LAST
#in.startCapital = 100000
#cashRate = 0.05
#Cost = 0.0064

runBacktest <- function(in.wght,in.close,in.startCapital,cashRate=0.05,Cost=0.0064)
{
    #Match in.close to in.wght:
    in.close = in.close[row.names(in.wght),]
  
    #Initial matrices:
    in.close = in.close/100         #Convert cents to rands
    
    #Add First Row.
    in.close = rbind(array(NA,dim=c(1,ncol(in.close))),in.close)
    rownames(in.close)[1] = as.character(as.Date(rownames(in.close)[2])-1)
    in.wght = rbind(array(NA,dim=c(1,ncol(in.wght))),in.wght)
    rownames(in.wght)[1] = as.character(as.Date(rownames(in.wght)[2])-1)
    
    ## "Improve: check same number of rows and columns in in.close and in.wght"
    ## "Match Rows and Columns of in.close and in.wght"
    
    #Create Portfolio matrix:
    port.out <- array(0,dim = c(nrow(in.close), 3))
    row.names(port.out) = row.names(in.close)
    colnames(port.out) = c("Cash", "Equity", "Total")
    
    #Initialize Starting Capital:
    port.out[1, c("Cash", "Total")] = in.startCapital
    
    #Number of Shares:
    num.shares = array(0, dim=dim(in.wght))
    colnames(num.shares) = colnames(in.wght)
    rownames(num.shares) = rownames(in.wght)
    
    #Current Weight:
    cur.wght = array(0, dim=dim(in.wght))
    colnames(cur.wght) = colnames(in.wght)
    rownames(cur.wght) = rownames(in.wght)
    
    #Start Going Through each bar:
    for(k in 2:nrow(in.wght))          
    {
    #Movements on Current Portfolio:
      
      ## "Improve: Add Interest"
      port.out[k, "Cash"] = port.out[(k-1), "Cash"]*((1+cashRate)^(1/365)) #Brings Forward Last Cash Balance.
      num.shares[k,] = num.shares[k-1,]             #Bring Forward the number of Shares.
      
      ## "Improve: Include Dividends"
      
      ## Calculate only those in our universe at this bar:
      uni.pos = which(!is.na(in.close[k,]) & in.close[k,] != 0)
      
      ## Current Position:
      #What if a share goes out of the universe before it is sold to cash.
      port.out[k, "Equity"] = sum(num.shares[k,uni.pos]*in.close[k,uni.pos])
      port.out[k,"Total"] = port.out[k, "Cash"] + port.out[k, "Equity"]
      cur.wght[k,uni.pos] = as.matrix((num.shares[k,uni.pos] * in.close[k,uni.pos])/port.out[k,"Total"])
    
    #CHECK FOR CHANGES AND EXECUTE IF ANY:
      if(sum(is.na(in.wght[k,])) < length(in.wght[k,]))
      {
        ## "Improve: Add transaction Costs."
        totalCost = sum((as.integer((in.wght[k,uni.pos]*port.out[k,"Total"])/in.close[k,uni.pos]) - num.shares[k,uni.pos])*in.close[k,uni.pos])*Cost
        
        #Execute Changes:
        num.shares[k,uni.pos] = num.shares[k,uni.pos] + (as.integer((in.wght[k,uni.pos]*port.out[k,"Total"])/in.close[k,uni.pos]) - num.shares[k,uni.pos])
        
        #Result from changes
        port.out[k, "Equity"] = sum( (num.shares[k,uni.pos]*in.close[k,uni.pos]) ,na.rm = TRUE)
        port.out[k,"Total"] =  port.out[k,"Total"] - totalCost
        port.out[k,"Cash"] = port.out[k,"Total"] - port.out[k, "Equity"]
      }   
      
    }
    
    return(port.out)
}

#portOut = portConstruct(portWeights[,,1],close.mat,100000,0.05,0.0054)

#exportPath <- "C:\\Users\\Paul Nheera\\Documents\\R Scripts\\Functions\\"
#outFile <- "Portfolio Back Test Out Put2.xlsx"

#outBook <- loadWorkbook(paste0(exportPath,outFile),create = TRUE)

#createSheet(outBook,"Sheet1")
#writeWorksheet(outBook,data = portOut,sheet = "Sheet1",startRow = 1,startCol = 1,
#               header = TRUE)
#saveWorkbook(outBook,paste0(exportPath,outFile))

#createSheet(outBook,"Sheet2")
#writeWorksheet(outBook,data = portOut,sheet = "Sheet2",startRow = 1,startCol = 1,
#               header = TRUE)

#saveWorkbook(outBook,paste0(exportPath,outFile))
