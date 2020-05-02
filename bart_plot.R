library(tidyverse)
library(ggplot2)
library(ggthemes)
library(xts)
library(highfrequency)
library(lubridate)

setwd() #assign wd based on where output of loop is

output <- read.csv(" ", header = TRUE) #load in file
output$Time <- as.POSIXct(output$Time)
#plots price over time
plot(x = output$Time, y = output$Price, type = "l", xlab = "Time", ylab = "Price", xaxt = "n")
#setting up axes
r <- as.POSIXct(round(range(output$Time), "days"))
t <- as.POSIXct(round(range(output$Time), "hours"))
axis.POSIXct(3, at = seq(r[1], r[2], by = "day"), format = "%D")
axis.POSIXct(1, at = seq(t[1], t[2], by = "6 hour"), format = "%H")

starts <- which(output$BartStart ==1)
if (length(starts)> 0) {
  for (i in 1:length(starts)){
    #adds start and end points
    points(x = output$Time[starts][i], y = output$Price[starts][i], col = i, pch = 19) #start
    points(x = output$Time[starts[i] + output$BartEndRowDiff[starts[i]]],
            y = output$Price[starts[i] + output$BartEndRowDiff[starts[i]]], col = i, pch=19) #end
    #adds text to either left or right side of point to avoid cluttering
    if (i%%2 == 0){
      text(x = output$Time[starts][i], y = output$Price[starts][i], col = i, label = i, pos=4, cex=.8) #start
      text(x = output$Time[starts[i] + output$BartEndRowDiff[starts[i]]],
           y = output$Price[starts[i] + output$BartEndRowDiff[starts[i]]], col = i, label=i, pos=4, cex=.8) #end
    } else if (i%%2 == 1){
      text(x = output$Time[starts][i], y = output$Price[starts][i], col = i, label = i, pos=2, cex=.8) #start
      text(x = output$Time[starts[i] + output$BartEndRowDiff[starts[i]]],
           y = output$Price[starts[i] + output$BartEndRowDiff[starts[i]]], col = i, label=i, pos=2, cex=.8) #end
    }
    
  }
}
  


