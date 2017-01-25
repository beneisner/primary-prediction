library(maps)
library(parallel)
library(doMC)
library(caret)
library(JuiceBox)
library(parallelSVM)
library(data.table)

setwd("/Users/Joshi/Dev/primary-prediction/")

train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

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


testResponse_df <- data.frame(testResponse)


twitterData_df[50,5:20]
twitterData <- fread("data/other/twitter_sentiment_scores.csv")
twitterData_df <- data.frame(twitterData)

twitterData_df_trump <- data.frame(as.numeric(twitterData_df[50,5:20]))
colnames(twitterData_df_trump) <- c("trump_sentiment_florida")

fill <- "gold1"
line <- "goldenrod2"

p8 <- ggplot(twitterData_df_trump, aes(x = trump_sentiment_florida)) +
  geom_density(fill = fill, colour = line) +
  scale_x_continuous(name = "",
                     breaks = seq(-1, 1, 0.1),
                     limits=c(-1.5, 1.5)) +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of twitter sentiment for Donald Trump in Florida")
  
p8 <- p8 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(fill='black', colour='black'))

fit <- readRDS("RDS/svmRadialFit2")
preds <- as.numeric(as.character(predict(fit, newdata = testMat)))







test_df$vote_fraction_residual <- test_df$fraction_votes - test_df$svm_pred

republican_plot_df <- test_df[which(test_df$party == 0),]
republican_plot_df <- republican_plot_df[c("candidate", "vote_fraction_residual")]

dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5))
                  , lines = rep(c("a", "b"), each = 100))
#Plot.
repub_plot <- ggplot(republican_plot_df, aes(x = vote_fraction_residual, fill = candidate)) + geom_density(alpha = 0.5) + theme(plot.background = element_rect(fill = 'black', colour = 'black'))
repub_plot <- repub_plot + theme(panel.background = element_rect(fill = 'black', colour = 'black'))
repub_plot <- repub_plot + theme(legend.background = element_rect(fill = 'black', colour = 'black'))
repub_plot <- repub_plot + theme(text=element_text(colour="white"))
repub_plot <- repub_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
repub_plot <- repub_plot + theme(axis.text.x=element_text(colour="white"))
repub_plot <- repub_plot + theme(axis.text.y=element_text(colour="white"))
repub_plot <- repub_plot + xlim(-0.45, 0.85) + xlab("Actual Vote % - Predicted Vote %")
repub_plot


democrat_plot_df <- test_df[which(test_df$party == 1),]
democrat_plot_df <- democrat_plot_df[c("candidate", "vote_fraction_residual")]

#Plot.
democrat_plot <- ggplot(democrat_plot_df, aes(x = vote_fraction_residual, fill = candidate)) + geom_density(alpha = 0.5) + theme(plot.background = element_rect(fill = 'black', colour = 'black'))
democrat_plot <- democrat_plot + theme(panel.background = element_rect(fill = 'black', colour = 'black'))
democrat_plot <- democrat_plot + theme(legend.background = element_rect(fill = 'black', colour = 'black'))
democrat_plot <- democrat_plot + theme(text=element_text(colour="white"))
democrat_plot <- democrat_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
democrat_plot <- democrat_plot + theme(axis.text.x=element_text(colour="white"))
democrat_plot <- democrat_plot + theme(axis.text.y=element_text(colour="white"))
democrat_plot <- democrat_plot + xlim(-0.45, 0.85) + xlab("Actual Vote % - Predicted Vote %")

democrat_plot

