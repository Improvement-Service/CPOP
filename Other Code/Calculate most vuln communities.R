IGZ_latest_sum <- IGZ_latest %>% group_by(InterZone) %>% dplyr::summarise(total = sum(CPPScore)) %>%
  left_join(IGZ_latest[1:3], by = "InterZone")
IGZ_latest_sum <- unique(IGZ_latest_sum) %>% group_by(CPP)%>%
  dplyr::mutate(rank = dense_rank(desc(total)))

saveRDS(IGZ_latest_sum, "C:/Users/cassidy.nicholas/Desktop/IGZRanks.rds")


test <- readRDS("C:/Users/cassidy.nicholas/Desktop/IGZRanks.rds")

test <- test %>% group_by(CPP) %>% dplyr::mutate(revRank = dense_rank(total)) 
write_csv(test, "C:/Users/cassidy.nicholas/Desktop/IGZRanks.csv")