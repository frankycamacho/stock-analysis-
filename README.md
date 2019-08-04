# stock-analysis-
stock analysis for PDC and Noble stock 
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#Get stock data for noble(NBL) and PDC(PDCE) 
getSymbols("NBL")
getSymbols("PDCE")

#Performance Charts for Noble

NBL%>%Ad()%>%chartSeries()
NBL%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2019')

#Performance Charts for PDC

PDCE%>%Ad()%>%chartSeries()
PDCE%>%chartSeries(TA=‘addBBands();addVo();addMACD()',subset='2019')

# Summary Statistics for Noble and PDC

summary(NBL)
summary(PDCE)

#20,50, and 200 moving average for Noble

candleChart(NBL, up.col = "black", dn.col = "red", theme = "white", subset = "2019")
addSMA(n = c(20, 50, 200))

#20,50, and 200 moving average for PDCE

candleChart(PDCE, up.col = "black", dn.col = "red", theme = "white", subset = "2019")
addSMA(n = c(20, 50, 200))

# Plot comparing stock returns for Noble and PDC

stocks <- as.xts(data.frame(NBL = NBL[, "NBL.Close"], PDCE = PDCE[, "PDCE.Close"]))

stock_return = apply(stocks, 1, function(x) {x / stocks[1,]}) %>% 
  t %>% as.xts

plot(as.zoo(stock_return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Return")
legend("topleft", c("NBL", "PDCE"), lty = 1:3, cex = 0.5)

# T-test and correlation tests for Noble and PDC 

#Get stock data return for PDC and Noble from 2007 to 2019 07/25
write.csv(stock_return, "stock_ret")
read.csv("stock_ret", row.names = 1)
data3 <-read.csv("stock_ret", row.names = 1)
head(data3)
summary(data3)

cor.test(data3$NBL.Close, data3$PDCE.Close)

t.test(data3$NBL.Close, data3$PDCE.Close)

wilcox.test(data3$NBL.Close, data3$PDCE.Close)

#Get stock data closing prices for Noble and PDC

write.csv(stocks, "stocks.csv")
read.csv("stocks.csv", row.names = 1)
data2 <-read.csv("stocks.csv", row.names = 1)
head(data2)

cor.test(data2$NBL.Close, data2$PDCE.Close)

t.test(data2$NBL.Close, data2$PDCE.Close)

t.test(data2$NBL.Close, data2$PDCE.Close, var.equal=FALSE)

wilcox.test(data2$NBL.Close, data2$PDCE.Close, conf.int = TRUE)


#Data Analytics Table Stats on stock returns 

table.Stats(stock_return$NBL.Close)
table.Stats(stock_return$PDCE.Close)

# Kurtosis test on closing prices 

kurtosis(NBL)          
kurtosis(PDCE)
