library(tidyverse)
library(ggplot2)
library(ggthemes)
library(xts)
library(highfrequency)

setwd("") #setwd for original data source
path <- "" #path for original data source

file.names <- dir(path, pattern = ".csv")
for (i in 1:length(file.names)){
  d1 <- read.csv(file.names[i], header=TRUE)
  btcdata <- d1[which(d1$symbol=="XBTUSD"), ]
  datetime <- strptime(btcdata[,1], format="%Y-%m-%d D %H:%M:%S")
  btcprice <- as.xts(btcdata[,5], order.by=datetime)
  colnames(btcprice) <- c("Price")
  btcprice1m <- to.minutes(btcprice, "minute", k=1)
  btcprice1m <- btcprice1m[,"minute.Close"]
  colnames(btcprice1m) <- c("Price")
  df <- fortify(btcprice1m)
  write.csv(df, paste("", #paste location for new data to be stored
                  file.names[i]), row.names = FALSE)
}

