#!/usr/local/bin/Rscript
#!/usr/bin/Rscript
#
# http://economistatlarge.com/portfolio-theory/r-optimized-portfolio/r-code-graph-efficient-frontier

#install.packages("stockPortfolio")

library(stockPortfolio)
library(quadprog)
library(reshape2)
library(ggplot2)

stocks <- c(
 "VTSMX" = .0,
 "SPY" = .20,
 "EFA" = .10,
 "IWM" = .10,
 "VWO" = .30,
 "LQD" = .20,
 "HYG" = .10)

stocks1 <- c(
  "VOO" = 0.20, # S&P 500 ETF
  "VUG" = 0.10, # Growth ETF
  "VIG" = 0.10, # Dividend Appreciation ETF
  "VYM" = 0.10, # High Dividen Yield ETF
  "VV"  = 0.10, # Large cap
  "MGC" = 0.10, # mega cap
  "MGK" = 0.10, # mega cap growth
  "MGV" = 0.10  # mega cap value
  )

#print(stocks)

# returns <- getReturns(names(stocks), freq="week")
# write.csv(returns$R, file = "etf-returns.csv")
# summary(returns)
# str(returns)

# Retrieve returns, from earliest start date possible (where all stocks have
# data) through most recent date
returns <- getReturns(names(stocks[-1]), freq="week") #Currently, drop index
#returns <- getReturns(names(stocks1), freq="week") #Currently, drop index

#### Efficient Frontier function ####
eff.frontier <- function (returns, short="no", max.allocation=NULL, risk.premium.up=.5, risk.increment=.005){
 # return argument should be a m x n matrix with one column per security
 # short argument is whether short-selling is allowed; default is no (short
 # selling prohibited)max.allocation is the maximum % allowed for any one
 # security (reduces concentration) risk.premium.up is the upper limit of the
 # risk premium modeled (see for loop below) and risk.increment is the
 # increment (by) value used in the for loop
 
 covariance <- cov(returns)
 print(covariance)
 n <- ncol(covariance)
 
 # Create initial Amat and bvec assuming only equality constraint
 # (short-selling is allowed, no allocation constraints)
 Amat <- matrix (1, nrow=n)
 bvec <- 1
 meq <- 1
 
 # Then modify the Amat and bvec if short-selling is prohibited
 if(short=="no"){
 Amat <- cbind(1, diag(n))
 bvec <- c(bvec, rep(0, n))
 }
 
 # And modify Amat and bvec if a max allocation (concentration) is specified
 if(!is.null(max.allocation)){
 if(max.allocation > 1 | max.allocation <0){
 stop("max.allocation must be greater than 0 and less than 1")
 }
 if(max.allocation * n < 1){
 stop("Need to set max.allocation higher; not enough assets to add to 1")
 }
 Amat <- cbind(Amat, -diag(n))
 bvec <- c(bvec, rep(-max.allocation, n))
 }
 
 # Calculate the number of loops
 loops <- risk.premium.up / risk.increment + 1
 loop <- 1
 
 # Initialize a matrix to contain allocation and statistics
 # This is not necessary, but speeds up processing and uses less memory
 eff <- matrix(nrow=loops, ncol=n+3)
 # Now I need to give the matrix column names
 colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
 
 # Loop through the quadratic program solver
 for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
 dvec <- colMeans(returns) * i # This moves the solution along the EF
 sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
 eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
 eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
 eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
 eff[loop,1:n] <- sol$solution
 loop <- loop+1
 }
 
 return(as.data.frame(eff))
}
 
# Run the eff.frontier function based on no short and 50% alloc. restrictions
eff <- eff.frontier(returns=returns$R, short="no", max.allocation=.50, risk.premium.up=1, risk.increment=.001)

print(head(eff))
 
# Find the optimal portfolio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]

print(eff.optimal.point)
 
# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
 geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
 color=ealred, size=5) +
 annotate(geom="text", x=eff.optimal.point$Std.Dev, y=eff.optimal.point$Exp.Return,
 label=paste("Risk: ",
 round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
 round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
 round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
 hjust=0, vjust=1.2) +
 ggtitle("Efficient Frontier\nand Optimal Portfolio") +
 labs(x="Risk (standard deviation of portfolio)", y="Return") +
 theme(panel.background=element_rect(fill=eallighttan),
 text=element_text(color=ealdark),
 plot.title=element_text(size=24, color=ealred))

ggsave("Efficient Frontier.png")

