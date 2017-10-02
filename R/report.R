##############################################################
#  set
##############################################################
source("./helper.R")
symbol.name <- c("USDJPYhour")
test.period <- c("2016-01-01::2017-07-01") # set test period

##############################################################
#  Comparison of cumulative return between the combined agent (Ensemble) and the aver- age of agents (Average)
##############################################################
# set data
load("../data/realtimeData2.RData")
test.data <- na.omit(realtimeData2[test.period])

# load cumsum return
ens <- readRDS("../data/result/USDJPYhour_ense_orderbook.rds")
avg <- readRDS("../data/result/USDJPYhour_avg_orderbook.rds")
temp <- cbind(ens$return_cumsum, avg$return_cumsum, test.data$close)
temp <- na.omit(na.locf(temp))[,1:2]

# plot cumulative returns
plot(temp[,1],minor.ticks=FALSE,major.format="%b-%Y", main ="", ylab = "cumulative return")  # ensemble
lines(temp[,2], col = 2)  # average
legend("topleft", legend=c("Ensemble", "Average"), lty=c(1, 1), col=1:2, bty="n")

##############################################################
#  Statistics of tested agents
##############################################################
### ensemble
tail(ens, 1)$return_cumsum  # total return
sum(ens$return > 0) / tail(ens, 1)$tradeID  # win rate
tail(ens, 1)$tradeID  # trades

### average
tail(avg[, 501], 1)  # total return
avg_sub <- avg[,-501]  # delete the cumsum return
sum(avg_sub > 0) / sum(avg_sub != 0) # win rate
sum(avg_sub != 0) / 500 # trades
