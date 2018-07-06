#Bloomberg Data:

#---- Load Libraries ----
library(Rblpapi)
library(quantmod)
source("Functions/get_2DTimeSeries.R")

#---- Connect to Bloomberg: ----
blpConnect()


#---- Get Data ----

#Index Tickers:
SPX.tickers = bds("SPX Index","INDX_MEMBERS")
saveRDS(SPX.tickers,"SPX-tickers.rds")

#Historical Data:
SPX = bdh(c(paste(as.matrix(SPX.tickers),"Equity"),"SPX Index"), field, 
          start.date = Sys.Date()-250,options = opt)

saveRDS(SPX,"SPX.rds")

#2D Matrix:
SPX.2D = get_2DTimeseries(SPX)
SPX.2D = SPX.2D[,c(sort(colnames(SPX.2D)[-which(colnames(SPX.2D)=="SPX Index")]),"SPX Index")]
SPX.2D = na.locf(SPX.2D)

saveRDS(SPX.2D,"SPX_2D.rds")

