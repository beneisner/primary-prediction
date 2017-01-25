library(parallel)
library(doMC)
library(caret)
library(JuiceBox)
library(parallelSVM)

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

valMat <- as.matrix(val_df[predictors])
valResponse <- as.matrix(val_df[response])

trainStartRow <- 1;
trainEndRow <- length(trainResponse)

valStartRow <- trainEndRow + 1;
valEndRow <- valStartRow + length(valResponse) - 1

testMat <- as.matrix(test_df[predictors])
testResponse <- as.matrix(test_df[response])

# done so that caret doesn't think this is classification
trainResponse <- ifelse(trainResponse == 0, trainResponse + 0.00001, trainResponse)
trainResponse <- ifelse(trainResponse == 1, trainResponse - 0.00001, trainResponse)
mydf_train <- cbind(trainMat, trainResponse)
mydf_val <- cbind(valMat, valResponse)
bigdf <- rbind(mydf_train, mydf_val)


set.seed(12345)
folds=8
repeats=1

tr <- trainControl(method='cv',
                   number = 8,
                   verboseIter=TRUE,
                   savePredictions = FALSE,
                   returnData = FALSE,
                   returnResamp = "final")

tr2 <- trainControl(method="cv", number=1, index=list(Fold1=c(trainStartRow:trainEndRow)), 
                          indexOut=list(Fold1=c(valStartRow:valEndRow)), savePredictions=FALSE, verboseIter=TRUE)


registerDoMC(8)

# Radial SVM
# svmRadialGrid <- expand.grid(sigma = c(0.001,0.005,0.01,0.05), C = c(0.25,0.5,1,2,4,8))
# svmRadialFit <- train(fraction_votes ~., data = bigdf,
#                 method = "svmRadial",
#                 tuneGrid = svmRadialGrid,
#                 trControl = tr2)
# 
# saveRDS(svmRadialFit, file = "RDS/svmRadialFit2")


# Linear SVM
svmLinearGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
svmLinearFit <- train(fraction_votes ~., data = bigdf,
                method = "svmLinear",
                tuneGrid = svmLinearGrid,
                trControl = tr2)

saveRDS(svmLinearFit, file = "RDS/svmLinearFit2")

# Poly SVM
svmPolyGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
svmPolyFit <- train(fraction_votes ~., data = bigdf,
                      method = "svmPoly",
                      trControl = tr2)

saveRDS(svmPolyFit, file = "RDS/svmPolyFit2")

# svmBaggedPipeline <- function(X_train, Y_train, X_test, Y_test, params) {
#   samplingSize_p <- params[1]
#   gamma_p <- params[2]
#   cost_p <- params[3]
#   svmBaggedFit <- eval(substitute(parallelSVM(x = X_train, y = Y_train,
#                               numberCores = 8, samplingSize = .samplingSize,
#                               gamma=.gamma, cost = .cost,
#                               verbose = TRUE, cachesize = 300), list(.samplingSize=samplingSize_p, .gamma = gamma_p, .cost = cost_p)))
#   preds <- eval(substitute(as.numeric(as.character(predict(svmBaggedFit, newdata = .X_test))), list(.X_test = X_test)))
#   sqrt(mean((preds-Y_test)^2))
# }
# 
# svmBaggedBestParam <- JuiceBoxCV_GridSearch(X_train = trainMat, Y_train = trainResponse, numFolds = 3,
#                                            numRepeats = 1, goParallel = "none", seedNum = 500,
#                                            verbose_p = TRUE, fitGrid = expand.grid(samplingSize = c(0.25,0.45,0.65,0.8),
#                                                                                    gamma = c(0.1),
#                                                                                    cost = c(0.1, 1, 2, 4)), fn = svmBaggedPipeline)
# saveRDS(svmBaggedBestParam, file= "RDS/svmBaggedBestParam")


# svmBaggedFit <- parallelSVM(x = trainMat, y = trainResponse,
#                    numberCores = 8, samplingSize = 0.75,
#                    gamma=0.015, cost = 1.8,
#                    verbose = TRUE, cachesize = 300)
# 
# saveRDS(svmBaggedFit, file = "RDS/svmBaggedFit")
# 
# fit <- readRDS("RDS/svmBaggedFit")
# preds <- as.numeric(as.character(predict(fit, newdata = valMat)))
# sqrt(mean((preds-valResponse)^2))


fit <- readRDS("RDS/svmLinearFit2")
preds <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds-testResponse)^2))


