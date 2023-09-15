##This needs to be run before vulnerable community calcs if we want to update the IGZs that
#are most vulnerable in the "Vulnerable Communities" page of the CPOP
#Otherwise it uses 2006 data to calculate the most vulnerable, so this can change it to 2007
##Therefore it can be used to replace what is saved in Vulnerable communities - 2006.xlsx
##THIS IS OPTIONAL AND IS NOT NORMALLY RUN

library(tidyverse)
library(plyr)
library(DT)
library(data.table)

#Store value for the start year and most recent year data is available, this needs to be changed when data is refreshed annually
StrtYear <- "2008/09"
RcntYear <- "2019/20"

CPPdta <- read_csv("data/CPPcleandata.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")
DZdta <- read_csv("data/DZcleandata.csv")

# Create CPP Scores and Type Scores for most recent years data----------


IGZdta <- IGZdta %>% mutate(`High is Positive?` = "Yes")
IGZdta$`High is Positive?`[IGZdta$Indicator %in% c("Child Poverty","Out of Work Benefits", 
                                                   "Crime Rate", "Emergency Admissions", 
                                                   "Early Mortality", "Depopulation")] <- "No"


IGZ_latest <- filter(IGZdta, Year == StrtYear)

# CPP Score

IGZ_latest <- ddply(
  IGZ_latest,. 
  (CPP, Indicator), 
  transform, 
  CPPMean = (mean(value))
)

IGZ_latest$Differences <- IGZ_latest$value - IGZ_latest$CPPMean

IGZ_latest <- ddply(
  IGZ_latest,. 
  (CPP, Indicator), 
  transform, 
  StdDev = (sd(value))
)

IGZ_latest$ZScore <- IGZ_latest$Differences / IGZ_latest$StdDev

# If high is bad multiply Z score by minus 1 to ensure direction is the same for all indicators

IGZ_latest$CPPScore <- IGZ_latest$ZScore

IGZ_latest$CPPScore[IGZ_latest$`High is Positive?` == "No"] <- 
  (IGZ_latest$CPPScore[IGZ_latest$`High is Positive?` =="No"]) * -1

IGZ_latest <- select(IGZ_latest, c(-CPPMean, -Differences, -StdDev, -ZScore))

IGZ_latest_sum <- IGZ_latest %>% group_by(InterZone) %>% dplyr::summarise(total = sum(CPPScore)) %>%
  left_join(IGZ_latest[1:3], by = "InterZone")
IGZ_latest_sum <- unique(IGZ_latest_sum) %>% group_by(CPP)%>%
  dplyr::mutate(rank = dense_rank(desc(total))) %>% dplyr::mutate(revRank = dense_rank(total)) 

saveRDS(IGZ_latest_sum, "C:/Users/cassidy.nicholas/Desktop/IGZRanks.rds")
