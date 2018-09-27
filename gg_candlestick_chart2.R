#********************
# Candlestick Chart
#********************

#---- Load Library ----
library(dplyr)
library(bdscale)
library(ggplot2)
library(quantmod)
library(magrittr)
library(scales)


#----

ggCandlestick_chart2 <- function(ts){
  
  ts <- head(ts,100)
  
  gdata <- data.frame(ts) %>% 
    set_names(c("Open","High","Low","Close","Volume")) %>% 
    mutate(Time=as.POSIXct(rownames(.)))
  
  gdata %>% ggplot(aes(x=Time, ymin=Low,ymax=High,lower=pmin(Open,Close),upper=pmax(Open,Close),
                       fill = Open < Close,group=Time,middle=pmin(Open,Close))) +
    geom_boxplot(stat='identity')+
    ggtitle('FX') +
    xlab('') +
    theme(legend.position = 'none') +
    scale_x_bd(business.dates = gdata$Time,max.major.breaks = 10)
  
}