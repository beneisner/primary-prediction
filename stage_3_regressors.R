library(parallel)
library(doMC)
library(caret)
library(JuiceBox)
library(parallelSVM)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

# fs_results <- readRDS("RDS/fs_results")
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


# GBM
# gbmFit <- train(fraction_votes ~., data = bigdf,
#                 method = "gbm",
#                 trControl = tr2)
# 
# saveRDS(gbmFit, file = "RDS/gbmFit")

# NN
# nnetFit <- train(fraction_votes ~., data = bigdf,
#                 method = "nnet",
#                 trControl = tr2)
# 
# saveRDS(nnetFit, file = "RDS/nnetFit")

# pcaNNet
# pcaNNetFit <- train(fraction_votes ~., data = bigdf,
#                  method = "pcaNNet",
#                  trControl = tr2)
# 
# saveRDS(pcaNNetFit, file = "RDS/pcaNNetFit")

# glmnet
# glmnetFit <- train(fraction_votes ~., data = bigdf,
#                     method = "glmnet",
#                     trControl = tr2)
# 
# saveRDS(glmnetFit, file = "RDS/glmnetFit")

# Random Forest
# rfFit <- train(fraction_votes ~., data = bigdf,
#                    method = "rf",
#                    tuneGrid = expand.grid(mtry = c(3,4,5)),
#                    trControl = tr2)
# 
# saveRDS(rfFit, file = "RDS/rfFit")

# Radial SVM
# svmRadialGrid <- expand.grid(sigma = c(0.001,0.005,0.01,0.05), C = c(0.25,0.5,1,2,4,8))
# svmRadialFit <- train(fraction_votes ~., data = bigdf,
#                 method = "svmRadial",
#                 tuneGrid = svmRadialGrid,
#                 trControl = tr2)
# 
# saveRDS(svmRadialFit, file = "RDS/svmRadialFit2")


# Linear SVM
# svmLinearGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
# svmLinearFit <- train(fraction_votes ~., data = bigdf,
#                 method = "svmLinear",
#                 tuneGrid = svmLinearGrid,
#                 trControl = tr2)
# 
# saveRDS(svmLinearFit, file = "RDS/svmLinearFit2")

# Poly SVM
# svmPolyGrid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4))
# svmPolyFit <- train(fraction_votes ~., data = bigdf,
#                       method = "svmPoly",
#                       trControl = tr2)
# 
# saveRDS(svmPolyFit, file = "RDS/svmPolyFit2")

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


# svmBaggedFit2 <- parallelSVM(x = trainMat, y = trainResponse,
#                    numberCores = 8, samplingSize = 0.73,
#                    gamma=0.001, cost = 1,
#                    verbose = TRUE, cachesize = 300)
# 
# saveRDS(svmBaggedFit2, file = "RDS/svmBaggedFit2")
# 
# fit <- readRDS("RDS/svmBaggedFit2")
# preds <- as.numeric(as.character(predict(fit, newdata = valMat)))
# sqrt(mean((preds-valResponse)^2))
# preds <- as.numeric(as.character(predict(fit, newdata = testMat)))
# sqrt(mean((preds-testResponse)^2))





# fit <- readRDS("RDS/svmPolyFit2")
# fit2 <- readRDS("RDS/svmRadialFit2")
# preds <- as.numeric(as.character(predict(fit, newdata = testMat)))
# preds2 <- as.numeric(as.character(predict(fit2, newdata = testMat)))
# sqrt(mean((preds2-testResponse)^2))
# sqrt(mean(((preds+preds2)/2-testResponse)^2))

fit <- readRDS("RDS/svmBaggedFit2")
print("For Bagged SVM")
preds_svm_bagged_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_svm_bagged_val-valResponse)^2))
preds_svm_bagged_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_svm_bagged_test-testResponse)^2))

fit <- readRDS("RDS/svmRadialFit2")
print("For SVM w/ Radial Kernel")
preds_svm_rbf_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_svm_rbf_val-valResponse)^2))
preds_svm_rbf_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_svm_rbf_test-testResponse)^2))

fit <- readRDS("RDS/svmPolyFit2")
print("For SVM w/ Poly Kernel")
preds_svm_poly_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_svm_poly_val-valResponse)^2))
preds_svm_poly_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_svm_poly_test-testResponse)^2))

fit <- readRDS("RDS/svmLinearFit2")
print("For SVM w/ Linear Kernel")
preds_svm_linear_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_svm_linear_val-valResponse)^2))
preds_svm_linear_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_svm_linear_test-testResponse)^2))

fit <- readRDS("RDS/nnetFit")
print("For a neural network")
preds_nn_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_nn_val-valResponse)^2))
preds_nn_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_nn_test-testResponse)^2))

fit <- readRDS("RDS/gbmFit")
print("For stochastic gradient boosting")
preds_gbm_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_gbm_val-valResponse)^2))
preds_gbm_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_gbm_test-testResponse)^2))

fit <- readRDS("RDS/glmnetFit")
print("For glmnet")
preds_glmnet_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_glmnet_val-valResponse)^2))
preds_glmnet_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_glmnet_test-testResponse)^2))

fit <- readRDS("RDS/rfFit")
print("For random forest")
preds_rf_val <- as.numeric(as.character(predict(fit, newdata = valMat)))
sqrt(mean((preds_rf_val-valResponse)^2))
preds_rf_test <- as.numeric(as.character(predict(fit, newdata = testMat)))
sqrt(mean((preds_rf_test-testResponse)^2))

cor(preds_glmnet_test, preds_svm_linear_test)

print("For model averaged")
sqrt(mean((rowMeans(cbind(preds_svm_rbf_val, preds_svm_poly_val, preds_svm_linear_val))-valResponse)^2))
sqrt(mean((rowMeans(cbind(preds_svm_rbf_test, preds_svm_poly_test, preds_svm_linear_test))-testResponse)^2))



