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








Prison <- read.csv("http://www.oberlin.edu/faculty/cdesante/assets/downloads/prison.csv")
head(Prison)

all_states <- map_data("state")
all_states
head(all_states)
Prison$region <- Prison$stateName
Total <- merge(all_states, Prison, by="region")
head(Total)
Total <- Total[Total$region!="district of columbia",]

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$bwRatio),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


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

