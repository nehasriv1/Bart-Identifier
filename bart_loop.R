library(tidyverse)
library(ggplot2)
library(ggthemes)
library(xts)
library(highfrequency)

setwd() #set wd based on where data is stored
path <- " " #path for data

file.names <- dir(path, pattern = ".csv")
for (i in 2:length(file.names)){
# Load in two days data
d1 <- read.csv(file.names[i-1], header=TRUE)
d2 <- read.csv(file.names[i], header=TRUE)
btcdata <- rbind(d1,d2)

#start of day 2 index
s <- which(substr(btcdata$Index, 1, 10) == substr(btcdata$Index[nrow(btcdata)], 1, 10))[1]
#noon of day 2 index
n <- which(substr(btcdata$Index, 1,13) == paste(substr(btcdata$Index[nrow(btcdata)],1,10), "12"))[1] 

# Find the max and min over next k1 minutes for day 1 and 12 hours of day 2
k1 <- 5
maxprice <- rollapply(btcdata$Price[1:n],k1,max,align = "left") #align left tells R to look ahead
minprice <- rollapply(btcdata$Price[1:n],k1,min,align = "left")

# merge this with the original price data
btcprice1m <- data.frame(btcdata$Index[1:(n-(k1-1))], btcdata$Price[1:(n-(k1-1))], maxprice, minprice,
                    abs(btcdata$Price[1:(n-(k1-1))] - maxprice),
                    abs(btcdata$Price[1:(n-(k1-1))] - minprice))
colnames(btcprice1m) <- c("Time", "Price", "MaxPrice", "MinPrice", "MaxDiff", "MinDiff")

# Find dates where price change in next k minutes is at least shift - the signficant change threshold
shift <- 35
shifts <- which(btcprice1m$MaxDiff>shift | btcprice1m$MinDiff>shift)
maxn <- nrow(btcprice1m)
#will show actual price change
change <- vector(, maxn)

change_fn <- function(x){
  return(x-x[1])
}
# Now, can just work on shifts
for (i in shifts) {
  z <- btcprice1m$Price[i:min(i+k1,maxn)] #window for price change
  z <- change_fn(z) #compute change
  zind <- min(which(abs(z)>shift))-1 #first value large enough
  change[i] <- z[zind+1] #this measures the change
}

starting_changes <- change[1:(s+k1)]

#for each price change, finds if there is a corresponding approx equal
#and oppoiste reaction within time frame
time_bound <- 720 #must be within 12 hours
lower_bound <- 15 #must be more than 15 min apart
cutoff <- 0.05 #within 5% of starting price
changeind <- which(starting_changes != 0)
barts <- matrix(nrow = length(changeind), ncol = 2) #will store indexes of start end end of bart
colnames(barts) <- c("start", "end")

for (i in 1:length(changeind)){
  delta <- change[changeind[i]] #actual change
  #looking for changes with magnitude between 95% and 105% of original change
  condition1 <- abs(change) >= (1-cutoff)*abs(delta)
  condition2 <- abs(change) <= (1+cutoff)*abs(delta)
  #must be change of opposite sign
  condition3 <- sign(delta) != sign(change) 
  potential_inds <- which(condition1&condition2&condition3)
  #incorporating time bounds
  potential_inds <- potential_inds[which((potential_inds <= time_bound+changeind[i])&(potential_inds > changeind[i]+lower_bound))]
  if (length(potential_inds) > 0) {
    barts[i,1] <- changeind[i]
    barts[i,2] <- potential_inds[1]
  }
}
barts <- barts[complete.cases(barts),]
#formatting bart results and reducing duplicate values
if(length(barts) ==0){
  barts <- matrix(rep(0, 2), ncol = 2)
} else if (length(barts) == 2){
  t <- matrix(nrow = 1, ncol = 2)
  colnames(t) <- c('start', 'end')
  t[1,] <- barts
  barts <- t
} else if (length(barts) > 2){
  barts <- barts[!duplicated(barts[,2]),]
  if (length(barts) == 2){
    t <- matrix(nrow = 1, ncol = 2)
    colnames(t) <- c('start', 'end')
    t[1,] <- barts
    barts <- t
  } else if(length(barts) >2 ){
    for (i in 1:nrow(barts)){
      group <- which(barts[,1]>barts[,1][i] & barts[,1]<=barts[,1][i]+10)
      if (length(group) == 0) next
      else if(length(group) > 0){
        barts[,1][i] <- round(mean(barts[,1][c(i, group)]),0)
        barts[,2][i] <- round(mean(barts[,2][c(i, group)]),0)
        barts[group, ] <- NA
      }
    }
    barts <- matrix(barts[complete.cases(barts),], ncol =2)
  } 
}


#output 
output <- btcprice1m[,c("Time", "Price")]
sig_change <- vector(, maxn)
sig_change[shifts] <- 1
start_change <- vector(, maxn)
start_change[barts[,1]] <- change[barts[,1]]
end_change <- vector(, maxn)
end_change[barts[,2]] <- change[barts[,2]]
direction <- vector(, maxn)
direction[barts[,1]] <- sign(change[barts[,1]])

output$sig_change <- sig_change
output$start_change <- start_change
output$end_change <- end_change

start <- vector(, maxn)
start <- as.numeric(output$start_change !=0)
output$bart_start <- start
output$bart_end_rows_below <- vector(,maxn)
output$bart_end_rows_below[barts[,1]] <- barts[,2]-barts[,1]
output$direction <- direction

colnames(output) <- c("Time", "Price","SuddenChange", "ChangeAtStart", "ChangeAtEnd", "BartStart", "BartEndRowDiff", "Direction")
file_name<- paste(substr(output$Time[1],1,10),"output.csv")
write.csv(output,paste(" ",file_name), #paste in output folder
          row.names = FALSE)
}


############################################

