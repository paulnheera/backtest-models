#Account
library(R6)
library(dplyr)

account <- R6Class("account",
  public = list(
    #Account Values:
    Balance = NULL,
    Equity = NULL,
    FreeMargin = NULL,
    UsedMargin = NULL,
    PnL = NULL,
    #Account Trades History:
    Trades = NULL,
    #Leverage:
    leverage = 100, #times
    #Risk Management:
    riskLimit = 0.05,
    stopLoss = 10, #pips
    
    initialize = function(Balance){
      self$Balance = Balance
      self$Equity = Balance
      self$FreeMargin = Balance
      self$UsedMargin = 0
      
      self$Trades = data.frame(time = character(),symbol = character(),
                          type = character(),size = numeric(),
                          price = numeric(),sl = numeric(),
                          tp = numeric(),PnL = numeric(),
                          status = character(),stringsAsFactors=FALSE)
    },
    
    createOrderEvent = function(signalEvent,dataHandler){
      time = signalEvent$date
      symbol = signalEvent$symbol
      type = signalEvent$type
      
      #Risk Limit is set at the opening of the Account.
      riskAmount = self$Balance * self$riskLimit
      
      #Stop Loss can either be set to a standar pip ammount. or Calculated from the
      # ATR or other stop loss methodologies.
      size = riskAmount/(self$stopLoss * 10)
      
      if(signalEvent$type == 'LONG'){
        price = dataHandler[which(dataHandler$Symbol == 'EURUSD'),]$Buy
        sl = price - self$stopLoss/10000
        tp = price + (self$stopLoss/10000)*2
      }else{
        price = dataHandler[which(dataHandler$Symbol == 'EURUSD'),]$Sell
        sl = price + self$stopLoss/10000
        tp = price - (self$stopLoss/10000)*2
      }
      
      #IMPROVE: Just add extra elements to signalEvent list.
      orderEvent = list(time = time,symbol = symbol,type = type,
                        size = size, price = price, sl = sl,tp = tp )
    },
    
    update = function(orderEvent,dataHandler){
      if(substr(orderEvent$symbol,4,6)=="USD"){
        print('Base Currency is USD')
        margin = (100000*orderEvent$size*orderEvent$price)/self$leverage
      }else{
        #Do Nothing for now!
      }
      
      if(margin > self$FreeMargin){
        cat("Insufficient Free Margin.\nCannot Execute Trade")
      }else{
        #Create Trade:
        self$Trades[nrow(self$Trades)+1,] <- c(orderEvent,NA,'ACTIVE')
        
        #Calculate PnLs for active trades:
        self$Trades = self$Trades %>%
          left_join(dataHandler,by = 'symbol') %>%
          mutate(PnL = ifelse(status == 'ACTIVE',
                          ifelse(type=='LONG',
                            round((100000*as.numeric(size)*(sell-as.numeric(price)))),
                            round(100000*as.numeric(size)*(as.numeric(price)-buy))),
                          PnL)
          ) %>%
          select(time:status)
      
        #Calaulate Active Profit:
        self$PnL = as.numeric(self$Trades %>%
          filter(status == 'ACTIVE') %>%
          summarise(sum(as.numeric(PnL)))
        )
        #Update Account:
        self$FreeMargin = self$FreeMargin-margin
        self$UsedMargin = self$UsedMargin + margin
        self$Equity = self$Balance + self$PnL
      }
    }
    
  )
)


#Test:
Account <- Account$new(500)
Account$Balance
Account$Equity
Account$FreeMargin
Account$UsedMargin

##Notes:
# Make the Trades data.frame an Active object. So it updates the Account fields.