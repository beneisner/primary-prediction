library(parallel)
library(doMC)
library(caret)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

predictors <- names(train_df)[which(!(names(train_df) %in% c("fraction_votes","votes","primary_date")))]
response <- c("fraction_votes")

trainMat <- as.matrix(train_df[predictors])
trainResponse <- as.matrix(train_df[response])

registerDoMC(8)
control <- rfeControl(functions=treebagFuncs, method="cv", number=8, 
                      verbose = TRUE, returnResamp = FALSE, saveDetails = FALSE)
results <- rfe(trainMat, trainResponse, sizes=seq(1,80,5), rfeControl=control)
saveRDS(results, file = "RDS/fs_results")


fs_results <- readRDS("RDS/fs_results")

