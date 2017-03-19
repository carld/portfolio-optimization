#!/usr/bin/Rscript
# data.r, downloads raw stock data
args <- commandArgs(TRUE)
extract <- function(args){
  stocks <- t(read.table(args[1],sep=",")[1,])
  J <- length(stocks)
  dir.create("data", showWarnings = FALSE)
  path <- "http://ichart.finance.yahoo.com/table.csv?s="
  for(j in 1:J){
      dat <- read.csv(paste(path,stocks[j],sep=""))
      write.csv(dat, file=paste("./data/",stocks[j],sep=""), row.names=FALSE, quote=FALSE)
  }
}

extract(args)

