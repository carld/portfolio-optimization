#!/usr/bin/Rscript
#
# ./optimization2.r $prices $N $R $Rmax
# T :- number of trading days
# R :- daily return for risk free asset
# N :- number of portfolios on the frontier
# RMax :- maxiumum daily return 
# prices: daily stock prices

args <- commandArgs(TRUE);
read_data <- function(args){
  data <- read.table(paste("./data/", args[1], sep=""),
          header=TRUE, sep=",")
  return(list(data, as.integer(args[2]), as.double(args[3]),
          as.double(args[4])));
}
returns <- function(data){
  dat <- data[[1]]; 
  N <- nrow(dat) - 1; 
  J <- ncol(dat); 
  ret <- (dat[1:N,1] - dat[2:(N+1),1])/dat[2:(N+1),1]; 
  if(J > 1){
      for(j in 2:J){
          ret <- cbind(ret, (dat[1:N,j] - dat[2:(N+1),j])/
                  dat[2:(N+1),j]);
  } }
  return(list(ret, names(data[[1]]), data[[2]],
          data[[3]], data[[4]]));
}

foptimization <- function(returns){
  p <- colMeans(returns[[1]]); # return values on the CML
  names(p) <- returns[[2]]; 
  J <- ncol(returns[[1]]); 
  M <- returns[[3]];
  R <- returns[[4]]; 
  Rmax <- returns[[5]];
  S <- cov(returns[[1]]); 
  Q <- solve(S); 
  u <- rep(1,J);
  a <- matrix(rep(0,4),nrow=2);
  a[1,1] <- u%*%Q%*%u;
  a[1,2] <- a[2,1] <- u%*%Q%*%p;
  a[2,2] <- p%*%Q%*%p;
  d <- a[1,1]*a[2,2] - a[1,2]*a[1,2];
  r <- seq(R, Rmax, length=M);
  s <- sqrt( a[1,1]*((r - a[1,2]/a[1,1])^2)/d + 1/a[1,1]); 
  ss <- sqrt(diag(S));
  cml <- c(sqrt(a[1,1]*R*R - 2*a[1,2]*R + a[2,2]), R);
  z <- (r - R)/cml[1]; 
  f <- Q%*%(p - R*u)/(cml[1]*cml[1]); 
  wcml <- matrix(rep(0,J*M), nrow=J); 
  wf <- rep(0,M);   # risk free asset weights on CML
  for(m in 1:M){
    wcml[,m] <- (r[m] - R)*f; 
    wf[m] <- 1 - wcml[,m]%*%u;
  }
  wcml <- rbind(wcml, t(wf));
  mp <- c(cml[1]/(a[1,2] - a[1,1]*R), (a[2,2] - a[1,2]*R)/(a[1,2] - a[1,1]*R)); 
  wmp <- Q%*%(p - R*u)/(a[1,2] - a[1,1]*R); # weights of MP
  minp <- c(sqrt(1/a[1,1]), cml[1]*sqrt(1/a[1,1]) + R); 
  wminp <- (minp[2] - R)*f; 
  wfminp <- 1- t(wminp)%*%u; 
  wminp <- rbind(wminp, wfminp);
  return(list(s, z, r, ss, p, cml, wcml, mp, wmp, minp, wminp));
}

plot_results <- function(data, returns, results){
  dat <- log(data[[1]]); 
  M <- nrow(dat); 
  J <- ncol(dat);
  ymax = max(dat); 
  ymin = min(dat); 
  mycolors <- rainbow(J); 
  s <- results[[1]]; 
  z <- results[[2]]; 
  r <- results[[3]]; 
  ss <- results[[4]]; 
  p <- results[[5]];
  cml <- results[[6]];
  mp <- results[[8]]; 
  wmp <- results[[9]];
  minp <- results[[10]]; 
  wminp <- results[[11]]; 
  postscript(file="./results2/fig1.eps", onefile=FALSE, horizontal=FALSE, height=10, width=5);
  par(mfrow=c(2,1));
  id <- c(1:nrow(dat));
  plot(id, rev(dat[,1]), ylim=c(ymin, ymax), type="l", col=mycolors[1], xlab="day", ylab="log(price)", main = "Asset Prices");
  if(J > 1){
    for(j in 2:J){
      lines(id, rev(dat[,j]),type="l",col=mycolors[j]);
    } 
  }
  legend("topleft", names(dat), cex=0.5, pch=rep(15, J), col=mycolors);
  ret <- returns[[1]]; 
  ymax = max(ret); 
  ymin = min(ret); 
  id <- c(1:nrow(ret)); 
  plot(id,rev(ret[,1]),ylim=c(ymin, ymax),type="l", col=mycolors[1], xlab="day", ylab="returns", main = "Asset Returns");
  if(J > 1){
    for(j in 2:J){
      lines(id, rev(ret[,j]),type="l",col=mycolors[j]);
    } 
  }
  legend("topleft", returns[[2]], cex=0.5, pch=rep(15, J), col=mycolors);
  postscript(file="./results2/fig2.eps", onefile=FALSE, horizontal=FALSE, height=10, width=5);
  par(mfrow=c(2,1));
  mycolors <- rainbow(length(p)+1);
  plot(s,r,xlim=c(0, max(s)),ylim=c(min(r, p),max(r, p)), type="l", col="blue", xlab="risk", ylab="return", main = "Capital Market Line, MVP2, MP");
  points(ss, p, pch=19, col=mycolors);
  text(ss, p, pos=4, cex=0.5, names(p));
  points(mp[1], mp[2], pch=19, col="black"); 
  points(mp[1], mp[2], pch=19, col="black"); 
  text(mp[1], mp[2], pos=2, cex=0.6, "MP"); 
  points(minp[1], minp[2], pch=19, col="black"); 
  text(minp[1], minp[2], pos=2, cex=0.6, "MVP2"); 
  text(0, cml[2], pos=4, cex=0.5, "RFA");
  lines(c(0, max(s)), c(cml[2], max(s)*cml[1] + cml[2]), lty=3);
  abline(h=0, lty=2); 
  abline(v=0, lty=2);
  f <- t(results[[7]]); 
  mycolors <- rainbow(J+1);
  plot(z,f[,1], xlim=c(0,max(z)), ylim=c(min(f),max(f)), type="l", col=mycolors[1], xlab="risk", ylab="portfolio weights", main="CML Portfolio Weights");
  if(J > 1){
    for(j in 2:J+1){
      lines(z,f[,j],type="l",col=mycolors[j]);
    }
  }
  abline(h=0, lty=2); 
  abline(v=mp[1], lty=3); 
  text(mp[1], min(f), pos=4, cex=0.5, "MP"); 
  abline(v=minp[1], lty=3);
  text(minp[1], min(f), pos=4, cex=0.5, "MVP2"); 
  legend("topleft", c(names(p), "RFA"), cex=0.5, pch=rep(15, J+1), col=mycolors); 
  postscript(file="./results2/fig3.eps", onefile=FALSE, horizontal=FALSE, height=10, width=5);
  par(mfrow=c(2,1));
  barplot(wminp, main="Minimum Variance Portfolio 2", xlab="Assets", ylab="Weights", col=mycolors, beside=TRUE);
  abline(h=0, lty=1);
  legend("topleft", c(names(p),"RFA"), cex=0.5, pch=rep(15, J+1), col=mycolors);
  barplot(wmp, main="Market Portfolio",
          xlab="Assets", ylab="Weights",
          col=mycolors, beside=TRUE);
  abline(h=0, lty=1);
  legend("topleft", names(p), cex=0.5, pch=rep(15, J), col=mycolors);
}

data <- read_data(args);
returns <- returns(data);
dir.create("results2", showWarnings = FALSE);
results <- foptimization(returns);
plot_results(data, returns, results);

