################################################################
# set  　　　　　　　　　　　　　　　　　　　　　　　
#################################################################
source("./helper.R")
symbol.name <- c("USDJPYhour")  
test.period <- c("2016-01-01::2017-12-31") # set test period

### Neural Network model (evalute market condition)
hidden.size <- 6
nn.model <- data.frame(RSI = rep(0, 3), diffADX = rep(0, 3), uniDMI = rep(0, 3),  # input  layer
                       Signal = c("-1", "0", "1")                                 # output layer
)                                
wts.size <- ncol(nn.model) * hidden.size + (hidden.size + 1) * 3  # number of connection

#################################################################
# apply test data
#################################################################
# set validation result
validResult <- readRDS(paste0("../data/result/", symbol.name, "_bestGeneration.rds"))

# set test data
load("../data/realtimeData2.RData")           # load historical data
test.data <- na.omit(realtimeData2[test.period])    # extract data of test period

# calclate technical indicators
indicators <- get_indicators(test.data) 

# apply test data: each agents
result <- sapply(1:500, function(x){
  validResult <- validResult[x,]
  
  # initialize classifers
  gtype <- gtype.ini(validResult)                                  # gtype
  signal.table <- signal.table.ini(validResult, data = test.data)  # signal.table
  position.table <- position.table.ini(data = test.data)           # position.table

  # apply test data
  for(step in 1:(nrow(test.data) - 1)){
    # generates the trading action each agent
    step.sig <- apply(gtype, 1, function(x){
      ID <- x[length(x)]
      x <- as.numeric(x[-length(x)])
      ind.sig(gtype = x,
            curr.price  = test.data[step,],
            curr.vola   = indicators[[4]][step, 1],
            signal      = signal.table[step, ID],
            posi.price  = position.table[step, c("open", "high", "low", "close")],
            posi.flag   = position.table[step,"posi.flag"]
      )  
    })
  
    # combine the output of agents
    ense.sig <- as.integer(mode(step.sig))
  
    # update the position
    position.table[step + 1,] <- updata.position(ense.sig,
                                               posi.price = position.table[step, c("open", "high", "low", "close")],
                                               posi.flag  = position.table[step,"posi.flag"],
                                               next.price = test.data[step +1,]
    )
   print(paste(step, " / " , nrow(test.data) - 1, ":", x))
  }

  # generates the orderbook
  orderbook <- cbind(test.data, position.table)
  orderbook$trade.flag <- abs(diff(orderbook$posi.flag))
  orderbook$exit.flag  <- ifelse(orderbook$trade.flag == 1 & orderbook$posi.flag  == 0, 1, 0)
  orderbook$entry.flag <- ifelse(orderbook$trade.flag == 1 & orderbook$exit.flag  == 0, 1, 0)

  orderbook.temp <- orderbook[orderbook$trade.flag == 1,]
  orderbook.temp$tradeID <- cumsum(orderbook.temp$entry.flag)
  orderbook.temp <- is.exit(orderbook.temp)
  orderbook.temp$diff.price <- diff(orderbook.temp$close)

  orderbook.temp$posi.temp  <- c(0, orderbook.temp$posi.flag[-length(orderbook.temp$posi.flag)])
  orderbook.temp$diff.price * orderbook.temp$posi.temp
})

#################################################################
# save the result
#################################################################
avoil.zero <- function(x) {
  if(length(x) == 0) x[1] <- 0  
  x
}
temp <- sapply(result, avoil.zero)

result2 <- na.fill(do.call(cbind, temp), 0)
mean.return <- apply(result2, 1, mean)
result2$return_cumsum <- cumsum(mean.return)

plot(result2$return_cumsum, type = "l")
saveRDS(result2, paste0("../data/result/",symbol.name, "_avg_orderbook.rds"))
