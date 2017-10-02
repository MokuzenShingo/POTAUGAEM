##############################################################
#  select the generation for testing
##############################################################
source("./helper.R")
trainResults <- readRDS("../data/result/USDJPYhour_valid500.rds")
symbol.name <- c("USDJPYhour")

dat <- data.frame(trainID  = trainResults$trainID,
                  generation     = trainResults$generation,
                  train.fit = trainResults$train.fit,
                  valid.fit = trainResults$valid.fit
)
dat$train.fit_year <- dat$train.fit / 10
dat$valid.fit_year <- dat$valid.fit 

# calculate the mean fitness for each generation
train.fit_gene_mean <- tapply(dat$train.fit_year, INDEX = as.factor(dat$generation), mean)
valid.fit_gene_mean <- tapply(dat$valid.fit_year, INDEX = as.factor(dat$generation), mean)

# select the generation for testing
bestGeneration <- which.max(valid.fit_gene_mean) 

# draw the figure
par(mar=c(4,4,1,4))
plot(valid.fit_gene_mean, type = "l", xlab = "generation", ylab="validation fitness")
par(new=TRUE)
plot(train.fit_gene_mean, type = "l", col = 2, xlab = "", ylab="", axes=FALSE)
axis(side=4)
mtext("training fitness", side = 4, line=3)
legend("bottomright", legend = c("validation", "training"), lty=c(1, 1), col=c(1, 2), bty="n")

# dat_bestGeneration <- dat[dat$generation == bestGeneration,]
# validTWR_mean <- cumsum(dat_bestGeneration$valid.fit_year) / 1:nrow(dat_bestGeneration)
# plot(validTWR_mean, type = "l", 
#      main = paste0("bestGeneration : ", bestGeneration),
#      xlab = "number of submodel")

#################################################################
# save the generation for testing
#################################################################
validResult <- trainResults[trainResults$generation == bestGeneration,]
saveRDS(validResult, paste0("../data/result/",symbol.name, "_bestGeneration.rds"))