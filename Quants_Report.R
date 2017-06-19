#Quantitative Investment Report:

func.path = "~/repos/backtest_models/"
strategy = "MoM_Strategy_GoogleData.R"
backtest = "portfolio_backtest.R"

## @Knitr Libraries
library(quantmod)
library(knitr)
library(PerformanceAnalytics)
library(rmarkdown)

## @knitr Test
print('This is a test')

## @Knitr Load_Data
Close = Close

## @Knitr SX40 if its not in the env.:
if(!exists("JSE:STX40")) getSymbols.google("JSE:STX40",env=globalenv())
bench = `JSE:STX40`[,4]

## @Knitr Source_Functions
source(paste0(func.path,strategy))
source(paste0(func.path,backtest))

## @Knitr Input
startDate = "2007-01-01"

## @Knitr Start Report
portWghts = MoM_strat(Close,9,3,3,startDate)

portOut = runBacktest(portWghts[,,1],Close,100000) # portNAVs =

portOut1 = runBacktest(portWghts[,,1],Close,100000)
portsNAVs = portOut1[,"Total",drop=FALSE]
for(i in 2:5){
  temp = runBacktest(portWghts[,,i],Close,100000)
  portsNAVs = cbind(portsNAVs,temp[,"Total",drop=F])
}
colnames(portsNAVs) = c("Quantile1","Quantile2","Quantile3","Quantile4",
                        "Quantile")

## @knitr Monthly
eomPoints = endpoints(portOut)
p1 = portOut[c(1,eomPoints),"Total",drop=F]
bench1 = as.matrix(bench[rownames(p1),,drop=F])
QNAVs = portsNAVs[c(1,eomPoints),,drop=F]

p1.ret = ROC(p1)
bench1.ret = ROC(bench1); colnames(bench1.ret) = "STX40"
QRet = ROC(QNAVs)

rtn = cbind(p1.ret,bench1.ret)
returns = cbind(QRet,bench1.ret)

## @knitr performance_chart
main.text = "9 Month Momentum Performance"
xlab = "Date"
ylab = "Cummulative Return"
clrs = rainbow(6)
text = colnames(returns)

chart.CumReturns(returns,col=clrs,
                 main = main.text,xlab=xlab,ylab=ylab)
legend("topleft",legend = text,col =clrs,
       lty=1,bty="n",
       cex=0.8,pt.cex = 1)


## @knitr Create_Report
rmarkdown::render("Quants_Report.Rmd")
