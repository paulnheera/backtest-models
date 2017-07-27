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
    
    #Trades = list(),
    
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



Account <- Account$new(500)
Account$Balance
Account$Equity
Account$FreeMargin
Account$UsedMargin
Account$SL
Account$calc_market_order('EURUSD','Short')




##Notes:
# Make the Trades data.frame an Active object. So it updates the Account fields.