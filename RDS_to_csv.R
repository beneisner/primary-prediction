train_df <- readRDS("RDS/train_df")
val_df <- readRDS("RDS/val_df")
test_df <- readRDS("RDS/test_df")

write.table(train_df, file = "data/other/train_df.csv",sep = ",", row.names = FALSE, col.names = TRUE)
write.table(val_df, file = "data/other/val_df.csv",sep = ",", row.names = FALSE, col.names = TRUE)
write.table(test_df, file = "data/other/test_df.csv",sep = ",", row.names = FALSE, col.names = TRUE)