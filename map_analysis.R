library(maps)
library(parallel)
library(doMC)
library(caret)
library(JuiceBox)
library(parallelSVM)
library(data.table)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_analysis_df")
val_df <- readRDS("RDS/val_analysis_df")
test_df <- readRDS("RDS/test_analysis_df")

fs_var <- readRDS("RDS/fs_var")

predictors <- fs_var
response <- c("fraction_votes")

trainMat <- as.matrix(train_df[predictors])
trainResponse <- as.matrix(train_df[response])

valMat <- as.matrix(val_df[predictors])
valResponse <- as.matrix(val_df[response])

trainStartRow <- 1;
trainEndRow <- length(trainResponse)

valStartRow <- trainEndRow + 1;
valEndRow <- valStartRow + length(valResponse) - 1

testMat <- as.matrix(test_df[predictors])
testResponse <- as.matrix(test_df[response])

fit <- readRDS("RDS/svmBaggedFit2")
print("For Bagged SVM")
preds_svm_bagged_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_svm_bagged_val-valResponse)^2))
preds_svm_bagged_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_svm_bagged_test-testResponse)^2))

test_df$svm_pred <- preds_svm_bagged_test
test_df$party <- ifelse(test_df$candidate == "Bernie Sanders" | test_df$candidate == "Hillary Clinton", 
                        1,0)
test_dt <- data.table(test_df)


county_scaling <- test_dt[,.(sum(svm_pred)),by = c("county","party")]
county_scaling_df <- data.frame(county_scaling)

test_df$svm_pred <- sapply(c(1:nrow(test_df)), function(x) {
  return(test_df$svm_pred[x] / county_scaling_df$V1[which(county_scaling_df$county == test_df$county[x] &
                                                          county_scaling_df$party == test_df$party[x])])
})

test_df$svm_votes <- sapply(c(1:nrow(test_df)), function(x) {
  return(round((test_df$votes[x]/(test_df$fraction_votes[x] + 0.000001)) * test_df$svm_pred[x]))
})

test_dt <- data.table(test_df)
us_map_info <- test_dt[,.(predicted_votes_received = sum(svm_votes), actual_votes_received = sum(votes)),by = c("state","candidate")]
us_map_info <- data.frame(us_map_info)



write.table(us_map_info, file = "data/other/us_map_info.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")



