#***************
# Optimization
#***************

#---- Load Libraries and Functions ----
library(quantmod)
library(PerformanceAnalytics)

source("momentum_strat.R")
source("portfolio_backtest.R")

#---- Load Data ----
jalsh_adjusted <- readRDS("~/repos/Google Finance Database/jalsh_adjusted.rds")

#---- Input Parameters ----

J_vector = c(3,6,9,12) # Formation Period
H_vector = c(3,6,9,12) # Holding Period

#---- Create Parameter Tables ----

ann_returns = matrix(NA,nrow = 4,ncol=4)
std_devs = matrix(NA,nrow = 4,ncol=4)
sharpe_ratios = matrix(NA,nrow = 4,ncol=4)

for(j in J_vector) for(h in H_vector){
  
  port_wghts = MoM_strat(jalsh_adjusted,j,h,h,startDate = "2008-01-01")
  port_navs = runBacktest(port_wghts[,,1],jalsh_adjusted,100000)
  
  ret = ROC(port_navs[,"Total"])
  
  #Ann Return:
  ann_returns[h/3,j/3] = Return.annualized(ret)
  
  #Std_Dev:
  std_devs[h/3,j/3] = StdDev.annualized(ret,scale=252)
  
  #Shape Ratio:
  sharpe_ratios[h/3,j/3] = SharpeRatio.annualized(ret)
  
}


