###############################################################################
# Load Systematic Investor Toolbox (SIT): Requires RCurl package
############################################################################### 
library(SIT)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')

tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
bt.prep(data, align='keep.all', dates='2004:12::')

#*****************************************************************
# Code Strategies
#******************************************************************
prices = data$prices  
n = ncol(prices)

models = list()

# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

#*****************************************************************
# Test
#****************************************************************** 
models = list()

models$combo = bt.aaa.combo(data, period.ends, n.top = 5,
                            n.mom = 180, n.vol = 20)

models$aaa = bt.aaa.minrisk(data, period.ends, n.top = 5,
                            n.mom = 180, n.vol = 20)

plotbt.custom.report.part1(models) 

#*****************************************************************
# Sensitivity Analysis: bt.aaa.combo / bt.aaa.minrisk
#****************************************************************** 
# length of momentum look back
mom.lens = ( 1 : 12 ) * 20
# length of volatility look back
vol.lens = ( 1 : 12 ) * 20


models = list()

# evaluate strategies
for(n.mom in mom.lens) {
  cat('MOM =', n.mom, '\n')
  
  for(n.vol in vol.lens) {
    cat('\tVOL =', n.vol, '\n')
    
    models[[ paste('M', n.mom, 'V', n.vol) ]] = 
      bt.aaa.combo(data, period.ends, n.top = 5,
                   n.mom = n.mom, n.vol = n.vol)
  }
}

out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)

#*****************************************************************
# Create Report
#****************************************************************** 
# allocate matrix to store backtest results
dummy = matrix('', len(vol.lens), len(mom.lens))
colnames(dummy) = paste('M', mom.lens)
rownames(dummy) = paste('V', vol.lens)

names = spl('Sharpe,Cagr,DVR,MaxDD')

layout(matrix(1:4,nrow=2))	
for(i in names) {
  dummy[] = ''
  
  for(n.mom in mom.lens)
    for(n.vol in vol.lens)
      dummy[paste('V', n.vol), paste('M', n.mom)] =
    out[i, paste('M', n.mom, 'V', n.vol) ]
  
  plot.table(dummy, smain = i, highlight = T, colorbar = F)
  
}
