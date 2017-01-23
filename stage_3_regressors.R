library(parallel)
library(doMC)
library(caret)
library(JuiceBox)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

fs_results <- readRDS("RDS/fs_results")
fs_var <- fs_results$optVariables
predictors <- fs_var
response <- c("fraction_votes")

trainMat <- as.matrix(train_df[predictors])
trainResponse <- as.matrix(train_df[response])

# done so that caret doesn't think this is classification
trainResponse <- ifelse(trainResponse == 0, trainResponse + 0.00001, trainResponse)
trainResponse <- ifelse(trainResponse == 1, trainResponse - 0.00001, trainResponse)
mydf <- cbind(trainMat, trainResponse)

set.seed(12345)
folds=8
repeats=1

tr <- trainControl(method='cv',
                   number = 8,
                   verboseIter=TRUE,
                   savePredictions = FALSE,
                   returnData = FALSE,
                   returnResamp = "final")


registerDoMC(8)



# Radial SVM
# svmRadialGrid <- expand.grid(sigma = c(0.1,0.2,0.4,0.8), C = c(0.25, 0.5, 1, 2, 4))
# 
# svmRadialFit <- train(fraction_votes ~., data = mydf,
#                 method = "svmRadial",
#                 tuneGrid = svmRadialGrid,
#                 trControl = tr)
# 
# saveRDS(svmRadialFit, file = "RDS/svmRadialFit")


# Linear SVM
# svmLinearGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
# svmLinearFit <- train(fraction_votes ~., data = mydf,
#                 method = "svmLinear",
#                 tuneGrid = svmLinearGrid,
#                 trControl = tr)
# 
# saveRDS(svmLinearFit, file = "RDS/svmLinearFit")

# Poly SVM
# svmPolyGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
svmPolyFit <- train(fraction_votes ~., data = mydf,
                      method = "svmPoly",
                      trControl = tr)

saveRDS(svmPolyFit, file = "RDS/svmPolyFit")
