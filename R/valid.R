################################################################
# set  　　　　　　　　　　　　　　　　　　　　　　　
#################################################################
source("./helper.R")
symbol.name <- c("USDJPYhour")  # set currency pairs : USDJPYhour, EURJPYhour, GBPJPYhour, EURUSDhour, GBPUSDhour, EURGBPhour
valid.period <- c("2015-01-01::2015-12-31") # set validation period

### Neural Network model (evalute market condition)
hidden.size <- 6
nn.model <- data.frame(RSI = rep(0, 3), diffADX = rep(0, 3), uniDMI = rep(0, 3),  # input  layer
                       Signal = c("-1", "0", "1")                                 # output layer
)                                
wts.size <- ncol(nn.model) * hidden.size + (hidden.size + 1) * 3  # number of connection

#################################################################
# apply validation data
#################################################################
# set training result
trainResults <- readRDS("../data/result/USDJPYhour_train500.rds")
trainResults <- trainResults[1:50000,]  ## valid_1 : 1:50000, valid_2 : 50001:100000, ... ,valid_5 : 200001:250000

# set validation data
load("../data/realtimeData2.RData")           # load historical data
valid.data <- na.omit(realtimeData2[valid.period])  # extract data of validation period

# calclate technical indicators
indicators <- get_indicators(valid.data)

# apply validation data
trainResults$valid.fit <- apply(trainResults[,3:53], 1, fitness, valid.data, hidden.size, nn.model, indicators)

#################################################################
# save the validation results
#################################################################
saveRDS(trainResults, paste0("../data/result/valid/",symbol.name, "_valid_1.rds"))  ## valid_1 to _5
