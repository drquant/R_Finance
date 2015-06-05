#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

# load saved Proxies Raw Data, data.proxy.raw
# please see http://systematicinvestor.github.io/Data-Proxy/ for more details
load('data/data.proxy.raw.Rdata')

N8.tickers = '
US.EQ = VTI + VTSMX + VFINX
EAFE = EFA + FWWFX + VGTSX
EMER.EQ = EEM + VEIEX
TECH.EQ = QQQ + ^NDX
JAPAN.EQ = EWJ + FJPNX
MID.TR = IEF + VFITX
US.CASH = SHY + VFISX,
US.HY = HYG + VWEHX
'

data = env()
getSymbols.extra(N8.tickers, src = 'yahoo', from = '1970-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

#*****************************************************************
# Run tests, monthly data - works
#*****************************************************************
data = bt.change.periodicity(data, periodicity = 'months')

plota.matplot(scale.one(data$prices))

prices = data$prices

# Need to load the kellerCLAfun script from a saved file source. Alternatively, you could include the 
# kellerCLAfun in this script. However, it is long and I chose to load it from a saved file source.
source("~/Documents/kellerCLAfun.R")

res = kellerCLAfun(prices, returnWeights = T, 0.25, 0.1, c('US.CASH', 'MID.TR'))

plotbt.transition.map(res[[1]]['2013::'])

plota(cumprod(1 + res[[2]]), type='l')

#*****************************************************************
# Create a benchmark
#*****************************************************************
models = list()  

commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)

data$weight[] = NA
data$weight$US.EQ = 1
data$weight[1:12,] = NA
models$US.EQ = bt.run.share(data, clean.signal=T, commission=commission, trade.summary=T, silent=T)

#*****************************************************************
# transform kellerCLAfun into model results
#*****************************************************************
#models$CLA = list(weight = res[[1]], ret = res[[2]], equity = cumprod(1 + res[[2]]), type = "weight")

obj = list(weights = list(CLA = res[[1]]), period.ends = index(res[[1]]))
models = c(models, create.strategies(obj, data, commission=commission, trade.summary=T, silent=T)$models)

#*****************************************************************
# Replicate using base SIT functionality
#*****************************************************************
weight.limit = data.frame(last(prices))
weight.limit[] = 0.25
weight.limit$US.CASH = weight.limit$MID.TR = 1

obj = portfolio.allocation.helper(data$prices, 
                                  periodicity = 'months', lookback.len = 12, silent=T, 
                                  const.ub = weight.limit,
                                  create.ia.fn = 	function(hist.returns, index, nperiod) {
                                    ia = create.ia(hist.returns, index, nperiod)
                                    ia$expected.return = (last(hist.returns,1) + colSums(last(hist.returns,3)) + 
                                                            colSums(last(hist.returns,6)) + colSums(last(hist.returns,12))) / 22
                                    ia
                                  },
                                  min.risk.fns = list(
                                    TRISK = target.risk.portfolio(target.risk = 0.1, annual.factor=12)
                                  )
)

models = c(models, create.strategies(obj, data, commission=commission, trade.summary=T, silent=T)$models)

#*****************************************************************
# Let's use Pierre's Averaged Input Assumptions 
#*****************************************************************
obj = portfolio.allocation.helper(data$prices, 
                                  periodicity = 'months', lookback.len = 12, silent=T, 
                                  const.ub = weight.limit,
                                  create.ia.fn = 	create.ia.averaged(c(1,3,6,12), 0),
                                  min.risk.fns = list(
                                    TRISK.AVG = target.risk.portfolio(target.risk = 0.1, annual.factor=12)
                                  )
)

models = c(models, create.strategies(obj, data, commission=commission, trade.summary=T, silent=T)$models)

#*****************************************************************
# Plot back-test
#*****************************************************************
models = bt.trim(models)
#strategy.performance.snapshoot(models, T)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))

layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
