###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
library(SIT)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   
tickers = spl('SPY,TLT,GLD,SHY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

# extend GLD with Gold.PM - London Gold afternoon fixing prices
data$GLD = extend.GLD(data$GLD)

bt.prep(data, align='remove.na')

#*****************************************************************
# Setup
#******************************************************************         
prices = data$prices   
n = ncol(prices)

period.ends = endpoints(prices, 'quarters')
period.ends = period.ends[period.ends > 0]       
period.ends = c(1, period.ends)


models = list()

#*****************************************************************
# Buy & Hold Benchmark Index
#****************************************************************** 
data$weight[] = NA
data$weight$SPY[period.ends,] = 1
models$SPY = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Dollar Weighted
#******************************************************************             
target.allocation = matrix(rep(1/n,n), nrow=1)
weight.dollar = ntop(prices, n)

data$weight[] = NA
data$weight[period.ends,] = weight.dollar[period.ends,]
models$dollar = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Dollar Weighted + 7% target volatility
#******************************************************************                 
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar,
                                                weight.dollar, 7/100, 21, 100/100)[period.ends,]
models$dollar.target7 = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Risk Weighted
#******************************************************************                 
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)  
weight.risk = weight.dollar / hist.vol
weight.risk = weight.risk / rowSums(weight.risk)

data$weight[] = NA
data$weight[period.ends,] = weight.risk[period.ends,]
models$risk = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Market Filter (tactical): 10 month moving average
#******************************************************************                 
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]       
period.ends = c(1, period.ends)

sma = bt.apply.matrix(prices, SMA, 200)
weight.dollar.tactical = weight.dollar * (prices > sma)  

data$weight[] = NA
data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
models$dollar.tactical = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Tactical + 7% target volatility
#******************************************************************                 
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
                                                weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
models$dollar.tactical.target7 = bt.run.share(data, clean.signal=F)


#*****************************************************************
# Create Report
#******************************************************************       
plotbt.custom.report.part1(models)       

plotbt.strategy.sidebyside(models)  

# Plot Portfolio Turnover for each Rebalancing method
layout(1:2)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
