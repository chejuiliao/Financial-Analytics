library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(forecast)
library(aTSA)
library(ccgarch)
library(fGarch)
library(rugarch)
library(stringr)
library(dplyr)

tickers = c("AAPL","AXP","BA","CAT","CSCO","CVX","DIS","DWDP","GS","HD",
            "IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE",
            "PFE","PG","TRV","UNH","UTX","V","VZ","WBA","WMT","XOM")

# get stock info
getSymbols(tickers)

# get stock close price
stocks_p <- cbind(AAPL[,4],	AXP[,4],	BA[,4],	CAT[,4],	CSCO[,4],	
                  CVX[,4],	DIS[,4],	DWDP[,4],	GS[,4],	HD[,4],	IBM[,4],	
                  INTC[,4],	JNJ[,4],	JPM[,4],	KO[,4],	MCD[,4],	MMM[,4],	
                  MRK[,4],	MSFT[,4],	NKE[,4],	PFE[,4],	PG[,4],	TRV[,4],	
                  UNH[,4],	UTX[,4],	V[,4],	VZ[,4],	WBA[,4],	WMT[,4], XOM[,4])

# calculate returns
stocks_r <- stocks_p
for (j in 1:ncol(stocks_r)) {
  stocks_r[, j] <- ROC(stocks_r[, j])
  colnames(stocks_r)[j] <- paste0(str_replace(colnames(stocks_r)[j], "Close", "Return"))
}

# the examining time period
stocks_r <- stocks_r["2017-02-01/2019-02-08"]

# take a look at the volatility of stocks
for(j in 1:ncol(stocks_r)) {
  print(plot(stocks_r[, j], col="black", main=paste0(colnames(stocks_r)[j], " Stock Return (Logarithmic)"), xlab="", ylab="Daily Returns", lwd=2, type="l"))
}

# Test for GARCH Effects and Normality - Functions cannot handle NA values #
arch_effect <- data.frame(stock = rep(NA, 30), LM_effect = rep(NA, 30))
for (j in 1:ncol(stocks_r)) {
  test <- arch.test(arima(stocks_r[, j], order = c(0,0,0)), output = TRUE)
  arch_effect$stock[j] <- colnames(stocks_r)[j]
  arch_effect$LM_effect[j] <- test[1,4]
}
arch_effect$stock <- str_replace(arch_effect$stock, ".Return", "")
arch_effect$LM_effect <- round(arch_effect$LM_effect, 0)
View(arch_effect)
# top five stocks with lowest p-value in Lagrange Multiplier 1 test
top_stocks <- stocks_r[, c("WMT.Return", "JNJ.Return", "PG.Return", "IBM.Return", "NKE.Return")]

# test for normal distribution
jb_test <- c()
for(j in 1:ncol(top_stocks)) {
  jb <- jb.test(top_stocks[, j])
  jb_test <- c(jb_test, jb[2,1])
}

# covariance of the stop stocks
top_stocks_cov <- cov(top_stocks)
# run GARCH models
# GARCH.t: 3, 4, 5
# Skew.GARCH.t: 1, 2
top_stocks_effect <- data.frame(stock = colnames(top_stocks), model = rep(NA, ncol(top_stocks)), alpha = rep(NA, ncol(top_stocks)), beta = rep(NA, ncol(top_stocks)))
for (j in 1:ncol(top_stocks)) {
  
  # for first two stocks, QGARCH-t is better
  if (j %in% c(1,2)) {
    GARCH.t <- garchFit(formula= ~ garch(1,1), data=top_stocks[, j], 
                        cond.dist="sstd", include.mean = FALSE)
    top_stocks_effect$model[j] <- "QGARCH-t"
  } 
  # for the rest of stocks, GARCH-t is better
  else{
    GARCH.t <- garchFit(formula= ~ garch(1,1), data=top_stocks[, j], 
                        cond.dist="std", include.mean = FALSE)
    top_stocks_effect$model[j] <- "GARCH-t"
  }
  # update the variance with predicted 5-day volatility
  top_stocks_cov[j, j] <- median(predict(GARCH.t)$meanError[1:5])^2
  # store the parameters
  top_stocks_effect$alpha[j] <- GARCH.t@fit$par[2]
  top_stocks_effect$beta[j] <- GARCH.t@fit$par[3]
  top_stocks_effect$AIC[j] <- GARCH.t@fit$ics["AIC"]
}

# calculate the historical median returns
med_return <- apply(top_stocks, MARGIN = 2, FUN = median)
top_stocks_effect <- cbind(top_stocks_effect, med_return)

View(top_stocks_effect)
library(gurobi)
library(prioritizr)

model <- list()

model$A     <- matrix(c(1,1,1,1,1, med_return),nrow=2,byrow=T)
model$Q     <- top_stocks_cov
model$obj   <- c(0,0,0,0,0)
model$rhs   <- c(1,0.0005)
model$sense <- c('=', '>=')
result <- gurobi(model,list())
names <- c()
for (i in 1:length(colnames(top_stocks))){
  names <- c(names, str_replace(colnames(top_stocks)[i], "Return", "Portion"))
}
names(result$x) <- names
result$objval  
result$x
sum(result$x*med_return)

# Efficient Frontier
mean.vec=med_return
cov.vec=top_stocks_cov

A.1     <- matrix(c(rep(1,length(mean.vec)),mean.vec),nrow=2,byrow=T)
A.2     <-diag(length(mean.vec))
model$A <- rbind(A.1,A.2)
model$Q     <- cov.vec
model$obj   <- rep(0,length(mean.vec))
model$sense <- c('=','=', rep('>=',length(mean.vec)))
param=seq(0.0004,0.0008, by=0.00002)
eff.front.weight=matrix(nrow=length(param),ncol=length(mean.vec))
Return=vector(length=length(param))
Risk=param
for (i in 1:length(param))
{
  print(i)
  model$rhs <- c(1,param[i],rep(0,length(mean.vec)))
  result <-gurobi(model,list())
  Return[i]=sum(result$x*mean.vec)
  Risk[i]=sqrt(result$objval)
  eff.front.weight[i,]=result$x
}
Return = Return * 100
Risk = Risk * 100
plot(Risk,Return,type='l', xlab = "Risk (%)", ylab = "Return (%)")

title("Efficient Frontier")
