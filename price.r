#!/usr/bin/Rscript
# price.r, extracts the stock prices
#
# ./price $stocks $T $file
# $stocks :- list of tickers
# $T :- number of trading days
# $file :- file to extract stock prices to

args <- commandArgs(TRUE)
extract_price <- function(args){
  stocks <- t(read.table(args[1], sep=",", strip.white=TRUE)[1,])
  J <- length(stocks)
  N <- as.integer(args[2])
  dat <- read.csv(paste("./data/", stocks[1], sep=""))
  price <- dat[1:N,5]
  if(J > 1){
      for(j in 2:J){
          dat <- read.csv(paste("./data/",
                  stocks[j], sep=""))
          price <- cbind(price, dat[1:N,5])
      }
  }
  write.table(price, file=paste("./data/", args[3], sep=""),
          row.names=FALSE, col.names=stocks,
          quote=FALSE, sep=",")
}
extract_price(args)
