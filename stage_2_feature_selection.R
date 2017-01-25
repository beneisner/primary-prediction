library(parallel)
library(doMC)
library(caret)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

predictors <- names(train_df)[which(!(names(train_df) %in% c("fraction_votes","votes","primary_date","state")))]
response <- c("fraction_votes")

trainMat <- as.matrix(train_df[predictors])
trainResponse <- as.matrix(train_df[response])

registerDoMC(8)
control <- rfeControl(functions=treebagFuncs, method="cv", number=8, 
                      verbose = TRUE, returnResamp = FALSE, saveDetails = FALSE)
results <- rfe(trainMat, trainResponse, sizes=seq(1,40,5), rfeControl=control)
saveRDS(results, file = "RDS/fs_results")

fs_results <- readRDS("RDS/fs_results")

# Adjust train
fraction_votes <- train_df$fraction_votes
votes <- train_df$votes
primary_date <- train_df$primary_date

train_df <- train_df[fs_results$optVariables]
train_df$fraction_votes <- fraction_votes
train_df$votes <- votes
train_df$primary_date <- primary_date

# Adjust val
fraction_votes <- val_df$fraction_votes
votes <- val_df$votes
primary_date <- val_df$primary_date

val_df <- val_df[fs_results$optVariables]
val_df$fraction_votes <- fraction_votes
val_df$votes <- votes
val_df$primary_date <- primary_date

# Adjust test
fraction_votes <- test_df$fraction_votes
votes <- test_df$votes
primary_date <- test_df$primary_date

test_df <- test_df[fs_results$optVariables]
test_df$fraction_votes <- fraction_votes
test_df$votes <- votes
test_df$primary_date <- primary_date

write.table(train_df, file = "data/other/train_df.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(val_df, file = "data/other/val_df.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(test_df, file = "data/other/test_df.csv", 
            row.names = FALSE, col.names = TRUE, sep = ",")







