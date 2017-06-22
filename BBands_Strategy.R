##Bollinger Bands:

library(quantmod)
library(PerformanceAnalytics)

ticker = "JSE:STX40"
getSymbols.google(ticker, env=globalenv())

px = get(ticker) 

portfolio = px
portfolio[,] = NA
portfolio = portfolio[,1:3]
colnames(portfolio) = c("Cash","Equity","Total")

no.Shares = px[,1]
no.Shares[,] =  0
colnames(no.Shares) = ticker

#Calculate Bollinger Bands:
Bbands = BBands(px[,4],n=20,maType = "SMA", sd=2)

startpos = 21
portfolio[startpos-1,"Cash"] = 10000
portfolio[startpos-1,"Equity"] = 0
portfolio[startpos-1,"Total"] = sum(portfolio[startpos-1,1:2])




#The Loop - Go Through Each Bar:
for(i in (startpos):nrow(px)){ #(Currently with one day lag in execution.)
  
  #carry over cash:
  portfolio[i,"Cash"] = portfolio[i-1,"Cash"]
  no.Shares[i] = no.Shares[i-1]
  
  #Check current positions 
  if(no.Shares[i] == 0){
    #Check for entry signal if = 0 shares:
    if(Bbands[i-1,"pctB"] < 0){
      #Buy Shares:
      no.Shares[i] = as.integer(portfolio[i,"Cash"]/px[i,4]) #NOTE: Change the 4 to Close.
      portfolio[i,"Equity"] = no.Shares[i]*px[i,4]
      portfolio[i,"Cash"] = portfolio[i,"Cash"] - portfolio[i,"Equity"]
      portfolio[i,"Total"] = portfolio[i,"Equity"] + portfolio[i,"Cash"]
    }else{
      no.Shares[i] = no.Shares[i-1]
      portfolio[i,"Equity"] = no.Shares[i]*px[i,4]
      portfolio[i,"Total"] = portfolio[i,"Equity"] + portfolio[i,"Cash"]
    }
      
  }else{
    #Check for exit signal if > 0 shares:
    if(Bbands[i-1,"pctB"] > 1){
      #Sell Shares:
      portfolio[i,"Equity"] = no.Shares[i]*px[i,4]
      portfolio[i,"Cash"] = portfolio[i,"Cash"] + portfolio[i,"Equity"]
      portfolio[i,"Equity"] = 0
      portfolio[i,"Total"] = portfolio[i,"Equity"] + portfolio[i,"Cash"]
      no.Shares[i] = 0
    }else{
      no.Shares[i] = no.Shares[i-1]
      portfolio[i,"Equity"] = no.Shares[i]*px[i,4]
      portfolio[i,"Total"] = portfolio[i,"Equity"] + portfolio[i,"Cash"]
    }
  }
  #Move on to the next bar:
}

startDate = index(portfolio)[startpos-1]
hold.ret = ROC(px[paste0(startDate,"::"),4])
strat.ret = ROC(portfolio[paste0(startDate,"::"),"Total"])

ret = merge.xts(hold.ret,strat.ret)

main.text = paste0(ticker)
xlabel = "Date"
ylabel = "Cummulative Return"

chart.CumReturns(ret,col=c("green","blue"),main=main.text,
                 xlab = xlabel, ylab=ylabel)
legend("topleft",legend = c("Buy and Hold","Strategy"),lty=1,
       col = c("green","blue"),cex=0.8,bty="n")

