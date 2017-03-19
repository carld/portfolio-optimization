#!/bin/sh

# File containing the list of stock symbols 
stocks="stocks.txt"
# Number of trading days
T="250"
# File containing the daily stock prices 
file="portfolio.txt"
# Number of portfolios
N="100"
# Daily return for the
R="0.0003"
# Maximum daily return
Rmax="0.01"
# The path to R
rpath=`which RScript`

#$rpath data.r $stocks
#$rpath price.r $stocks $T $file
#$rpath optimization1.r $file $N $Rmax
$rpath opt2.r $file $N $R $Rmax

