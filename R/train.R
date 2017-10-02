################################################################
# set  　　　　　　　　　　　　　　　　　　　　　　　
#################################################################
source("./helper.R")
symbol.name <- "USDJPYhour"  
train.period <- c("2010-01-01::2014-12-31") # set training period

### Neural Network model (evalute market condition)
hidden.size <- 6
nn.model <- data.frame(RSI = rep(0, 3), diffADX = rep(0, 3), uniDMI = rep(0, 3),  # input  layer
                       Signal = c("-1", "0", "1")                                 # output layer
)                                
wts.size <- ncol(nn.model) * hidden.size + (hidden.size + 1) * 3  # number of connection

### Genetic Algorithm
popSize <- 100  # set to population size
maxiter <- 500  # max iterations(generations) 

# gtype (search renge) :
# buy_benefit, buy_losscut, sell_benefit, sell_losscut, ADX length, RSI length, connection weights
min <- c( 1,  1,  1,  1,  8, 3, rep(0, wts.size))
max <- c(10, 10, 10, 10, 35, 8, rep(1, wts.size))

# crossoveer method
gaControl("real-valued" = list(crossover = "gareal_laplaceCrossover"))

#################################################################
# training
#################################################################
# set training data
load("../data/realtimeData2.RData")  # load historical data
train.data <- na.omit(realtimeData2[train.period])  # extract data of training period
train.data <- revers_trend(train.data)   # add revers trend

# calclate technical indicators
indicators <- get_indicators(train.data)

# performe genetic algorithm
GA <- ga(type = "real-valued", fitness = fitness,
         mktdata = train.data, hidden.size = hidden.size, nn.model = nn.model, indicators = indicators,
         min = min, max = max,
         keepBest = TRUE,
         popSize = popSize, maxiter = maxiter, pmutation = 0.2
)

#################################################################
# save the training results
#################################################################
# extract agents with the highest fitness for each generation
bestSol <- t(sapply(GA@bestSol, function(x) x[1,]))

# calculate the highest fitness for each generation
train.fit <- apply(bestSol, 1, fitness, train.data, hidden.size, nn.model, indicators)

# save the training results
trainResult <- data.frame(generation = 1:maxiter, train.fit, bestSol) # training results

output.name <- commandArgs(T)  # R --vanilla --args "output.name" < train.R
output.name <- as.integer(output.name)
saveRDS(trainResult, paste0("../data/result/train/",symbol.name, "_", output.name, ".rds"))