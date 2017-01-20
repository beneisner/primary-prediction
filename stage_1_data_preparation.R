library(data.table)
setwd("/Users/Joshi/Dev/primary-prediction/")
primary_results <- fread("data/election_results/primary_results.csv")
primary_dates <- fread("data/other/state_date_primary.csv")

colnames(primary_dates) <- c("state", "republican_primary_date", "democrat_primary_date")

