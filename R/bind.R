source("./helper.R")

##############################################################
#  perform bind
##############################################################
# training result
train_bind(trainDir = "../data/result/train/", outputFile.name =  "../data/result/USDJPYhour_train500.rds")

# validation result
valid_bind(validDir = "../data/result/valid/", outputFile.name =  "../data/result/USDJPYhour_valid500a.rds")
