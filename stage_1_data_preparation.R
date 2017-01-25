library(data.table)
library(bit64)
library(DMwR)
library(mice)
library(parallel)
library(doMC)
setwd("/Users/Joshi/Dev/primary-prediction/")
primary_results <- fread("data/election_results/primary_results.csv")
primary_dates <- fread("data/other/state_date_primary.csv")
colnames(primary_dates) <- c("state", "republican_primary_date", "democrat_primary_date")
additional_kaggle_features <- fread("data/election_results/county_facts.csv")

# Merge results and dates
primary_results <- merge(primary_results, primary_dates, by = "state")

# Merge results and additional kaggle features
primary_results <- merge(additional_kaggle_features, primary_results, by = "fips", all.y = TRUE)
primary_results_df <- data.frame(primary_results)

big10 <- 2000000000000000000
primary_results_df$MAN450207[which(primary_results_df$MAN450207 > 2000000000000000000)] = NA
primary_results_df$WTN220207[which(primary_results_df$WTN220207 > 2000000000000000000)] = NA
primary_results_df$RTN130207[which(primary_results_df$RTN130207 > 2000000000000000000)] = NA
primary_results_df$fips[which(is.na(primary_results_df$fips))] <- -1

primary_results_df$MAN450207 <- as.numeric(primary_results_df$MAN450207)
primary_results_df$WTN220207 <- as.numeric(primary_results_df$WTN220207)
primary_results_df$RTN130207 <- as.numeric(primary_results_df$RTN130207)

primary_date <- sapply(c(1:length(primary_results_df$party)), function(x) {
  party_name <- primary_results_df$party[x]
  if(x == "Democrat") {
    return(primary_results_df$democrat_primary_date[x])
  } else {
    return(primary_results_df$republican_primary_date[x])
  }
})

primary_results_df$democrat_primary_date <- NULL
primary_results_df$republican_primary_date <- NULL

primary_results_df$primary_date <- primary_date

# Remove Uncommitted, No Preference
primary_results_df <- primary_results_df[which(!(primary_results_df$candidate %in% c("Uncommitted", "No Preference"))),]
primary_results <- data.table(primary_results_df)

all_candidates <- unique(primary_results_df$candidate)
all_candidates_mod <- gsub('([[:punct:]])|\\s+','_',all_candidates)
all_candidates_mod <- paste0("is_running_", all_candidates_mod)

primary_candidate_to_date <- primary_results[,.(list(unique(primary_date))), by = candidate]

primary_date <- primary_results_df$primary_date
mini_df <- data.frame(primary_date)

registerDoMC(8)
for(i in all_candidates) {
  a <- unlist(primary_candidate_to_date$V1[which(primary_candidate_to_date$candidate == i)])
  mini_df[i] <- ifelse(mini_df$primary_date %in% a,1,0)
}

mini_df$primary_date <- NULL
colnames(mini_df) <- all_candidates_mod
mini_df$allCandRunning <- rowSums(mini_df)
primary_results_df <- cbind(primary_results_df, mini_df)

# Twitter Features
twitterData <- fread("data/other/twitter_sentiment_scores.csv")
twitterData_df <- data.frame(twitterData)

twitterFeatures <- apply(twitterData_df, 1, function(x){
  x <- x[c(5:ncol(twitterData_df))]
  a <- as.numeric(unlist(x[which(!is.na(x))]))
  if(length(a) > 0) {
    p_min <- min(a)
    p_25 <- quantile(a,0.25)
    p_median <- median(a)
    p_average <- mean(a)
    p_75 <- quantile(a, 0.75)
    p_max <- max(a)
    return(c(p_min, p_25, p_median, p_average, p_75, p_max))
  } else {
    return(c(NA, NA, NA, NA, NA, NA))
  }
})

twitterFeatures <- data.frame(t(twitterFeatures))

twitterData_df$Candidate <- ifelse(twitterData_df$Candidate == "Clinton", "Hillary Clinton", twitterData_df$Candidate)
twitterData_df$Candidate <- ifelse(twitterData_df$Candidate == "Sanders", "Bernie Sanders", twitterData_df$Candidate)
twitterData_df$Candidate <- ifelse(twitterData_df$Candidate == "Rubio", "Marco Rubio", twitterData_df$Candidate)
twitterData_df$Candidate <- ifelse(twitterData_df$Candidate == "Cruz", "Ted Cruz", twitterData_df$Candidate)
twitterData_df$Candidate <- ifelse(twitterData_df$Candidate == "Trump", "Donald Trump", twitterData_df$Candidate)
twitterFeatures$state <- twitterData_df$State 
twitterFeatures$candidate <- twitterData_df$Candidate 
colnames(twitterFeatures) <- c(paste0("twitter_", c("min", "25", "median", "mean", "75", "max")), "state", "candidate")

primary_results_df <- merge(twitterFeatures, primary_results_df, all.y = TRUE, by = c("state", "candidate"))

# Polling Data
pollingData_df <- data.frame(fread("data/poll/polls_aggregated.csv"))
pollingData_df$Party <- NULL
pollingData_df$Candidate <- ifelse(pollingData_df$Candidate == "Clinton", "Hillary Clinton", pollingData_df$Candidate)
pollingData_df$Candidate <- ifelse(pollingData_df$Candidate == "Sanders", "Bernie Sanders", pollingData_df$Candidate)
pollingData_df$Candidate <- ifelse(pollingData_df$Candidate == "Rubio", "Marco Rubio", pollingData_df$Candidate)
pollingData_df$Candidate <- ifelse(pollingData_df$Candidate == "Cruz", "Ted Cruz", pollingData_df$Candidate)
pollingData_df$Candidate <- ifelse(pollingData_df$Candidate == "Trump", "Donald Trump", pollingData_df$Candidate)
colnames(pollingData_df) <- c("state", "candidate", paste0("polling_", c("min", "25", "median", "75", "max", "mean", "avg_size")))
primary_results_df <- merge(pollingData_df, primary_results_df, all.y = TRUE, by = c("state", "candidate"))
primary_date <- primary_results_df$primary_date

# Preparing the dataset for NA removal
# na_count <-sapply(twitterData_df, function(y) sum(length(which(is.na(y)))))

# 0 represents democrat, 1 republican
primary_results_df$party <- as.numeric(as.factor(primary_results_df$party)) - 1

primary_results_df$area_name <- NULL
primary_results_df$state_abbreviation.x <- NULL
state <- primary_results_df$state
primary_results_df$state <- NULL
primary_results_df$state_abbreviation.y <- NULL
primary_results_df$county <- NULL

candidate <- primary_results_df$candidate
candidate_df <- data.frame(cbind(model.matrix(~. + 0, data = data.frame(candidate))))

colnames(candidate_df) <- c("Ben_Carson", "Bernie_Sanders", "Carly_Fiorina", 
                            "Chris_Christie", "Donald_Trump", "Hillary_Clinton",
                            "Jeb_Bush", "John_Kasich", "Marco_Rubio", 
                            "Martin_OMalley", "Mike_Huckabee", "Rand_Paul",
                            "Rick_Santorum", "Ted_Cruz"
                            )

primary_results_df <- cbind(primary_results_df, candidate_df)
primary_results_df$candidate <- NULL

fraction_votes <- primary_results_df$fraction_votes
votes <- primary_results_df$votes
primary_results_df$fraction_votes <- NULL
primary_results_df$votes <- NULL

# Replace NAs
a <- mice(primary_results_df,m=1,maxit=1,meth='cart',seed=500)
primary_results_df <- complete(a,1)
primary_results_df$PST040210[which(is.na(primary_results_df$PST040210))] <- median(primary_results_df$PST040210[which(!is.na(primary_results_df$PST040210))])
primary_results_df$POP010210[which(is.na(primary_results_df$POP010210))] <- median(primary_results_df$POP010210[which(!is.na(primary_results_df$POP010210))])
primary_results_df$polling_mean[which(is.na(primary_results_df$polling_mean))] <- median(primary_results_df$polling_mean[which(!is.na(primary_results_df$polling_mean))])

primary_results_df$fraction_votes <- fraction_votes
primary_results_df$votes <- votes
primary_results_df$primary_date <-  as.Date(primary_date, "%m/%d/%y")
primary_results_df$fips <- NULL

primary_results_df$state <- state
primary_results <- data.table(primary_results_df)

# a <- table(primary_results_df$primary_date)
# cumsum(a[order(names(a))])

# Training: 2016-02-01 through 2016-03-05
# Validation: 2016-03-08 through 2016-04-19
# Testing: 2016-04-26 through 2016-06-07

train_df <- primary_results_df[which(primary_results_df$primary_date <= "2016-03-05"),]
val_df <- primary_results_df[which(primary_results_df$primary_date >= "2016-03-08" & 
                                       primary_results_df$primary_date <= "2016-04-19"),]
test_df <- primary_results_df[which(primary_results_df$primary_date >= "2016-04-26"),]

saveRDS(train_df, file = "RDS/train_df")
saveRDS(val_df, file = "RDS/val_df")
saveRDS(test_df, file = "RDS/test_df")

