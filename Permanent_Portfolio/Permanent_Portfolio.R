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
# bt.max.deviation.rebalancing function
#****************************************************************** 
bt.max.deviation.rebalancing <- function
(
  data,
  model, 
  target.allocation, 
  max.deviation = 3/100, 
  rebalancing.ratio = 0,	# 0 means rebalance all-way to target.allocation
  # 0.5 means rebalance half-way to target.allocation
  start.index = 1,
  period.ends = 1:nrow(model$weight)							
) 
{
  nperiods = nrow(model$weight)
  action.index = rep(F, nperiods)
  
  start.index = period.ends[start.index]
  start.index0 = start.index
  
  while(T) {	
    # find rows that violate max.deviation
    weight = model$weight
    index = apply(abs(weight - rep.row(target.allocation, nperiods)), 1, max) > max.deviation
    index = which( index[period.ends] )
    
    if( len(index) > 0 ) {
      index = period.ends[index]
      index = index[ index > start.index ]
      
      if( len(index) > 0 ) {
        action.index[index[1]] = T
        
        data$weight[] = NA	
        data$weight[start.index0,] = target.allocation
        
        temp = rep.row(target.allocation, sum(action.index))
        data$weight[action.index,] = temp + 
          rebalancing.ratio * (weight[action.index,] - temp)					
        
        model = bt.run.share(data, clean.signal=F, silent=T)
        
        start.index = index[1]
      } else break			
    } else break		
  }
  return(model)
}

#*****************************************************************
# Code Strategies
#******************************************************************         
prices = data$prices   
n = ncol(prices)
nperiods = nrow(prices)

# annual
period.ends = endpoints(prices, 'years')
period.ends = period.ends[period.ends > 0]       
period.ends.y = c(1, period.ends)

# quarterly
period.ends = endpoints(prices, 'quarters')
period.ends = period.ends[period.ends > 0]       
period.ends.q = c(1, period.ends)

models = list()

#*****************************************************************
# Code Strategies
#******************************************************************     
target.allocation = matrix(rep(1/n,n), nrow=1)

# Buy & Hold    
data$weight[] = NA 
data$weight[period.ends.y[1],] = target.allocation
models$buy.hold = bt.run.share(data, clean.signal=F)

# Equal Weight Annual
data$weight[] = NA
data$weight[period.ends.y,] = ntop(prices[period.ends.y,], n)
models$equal.weight.y = bt.run.share(data, clean.signal=F)

# Equal Weight Quarterly
data$weight[] = NA
data$weight[period.ends.q,] = ntop(prices[period.ends.q,], n)
models$equal.weight.q = bt.run.share(data, clean.signal=F)

#*****************************************************************
# Rebalance based on threshold
#******************************************************************     
# Rebalance only when threshold is broken
models$threshold.y = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.y) 

# Rebalance only when threshold is broken
models$threshold.q = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.q) 

#*****************************************************************
# Create Report
#******************************************************************       
plotbt.custom.report.part1(models)       

plotbt.strategy.sidebyside(models)

# Plot Portfolio Turnover for each Rebalancing method
layout(1:2)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
