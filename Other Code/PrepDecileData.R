##prepare inequality data for table

##Run this after Create income deciles.R
library(tidyverse)
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")
decDta <- read_csv("data/IncomeDeciles.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")

IGZdta <- left_join(IGZdta, decDta, by = c("InterZone" = "AreaCode"))
##Make imputed data NAs and then remove them - in case 
#  we change our mind about how to display this in future
IGZdta[IGZdta$Type=="Imputed"|IGZdta$Type=="Projected", "value"] <- NA
IGZdta <- IGZdta[complete.cases(IGZdta$value),]
scotvals <- filter(IGZdta, ScotlandDec %in% c(1,10)) %>%
  group_by(ScotlandDec, Indicator, Year) %>%
  summarise_at(vars(value), list(mean))
scotvals$CouncilName <- "Scotland"
names(scotvals)[1] <- "Decile"

dd <- IGZdta[c(12,11,6,8:10)] %>% filter(Decile %in% c(1,10))%>%
  group_by(Decile,Year,CouncilName,Indicator) %>%
  summarise_at(vars(value), list(mean))
dd <- bind_rows(dd[1:5], scotvals[1:5])

##Previously we wanted to keep every year, but no longer! - get last year
#dd <- filter(dd, Year %in% c("2006/07", "2007/08", "2008/09", "2009/10", 
#                             "2010/11", "2011/12", "2012/13",
#                             "2013/14", "2014/15", "2015/16", "2016/17", "2017/18"))
dd <- dd %>% dplyr::arrange(Year) %>% group_by(Indicator)%>%
  filter(Year ==last(Year))

dd[dd$Indicator == "Child Poverty",4] <- "Child Poverty (%)"
dd[dd$Indicator == "Crime Rate",4] <- "Crime Rate, per 10,000"
dd[dd$Indicator == "Early Mortality",4] <- "Early Mortality, per 100,000"
dd[dd$Indicator == "Emergency Admissions",4] <- "Emergency Admissions, per 100,000"
dd[dd$Indicator == "Out of Work Benefits",4] <- "Out of Work Benefits (%)"
dd[dd$Indicator == "Positive Destinations",4] <- "Positive Destinations (%)"
dd[dd$Indicator == "Depopulation",4] <- "Depopulation Index"
dd[dd$Indicator == "Attainment",4] <- "Average Highest Attainment"
saveRDS(dd, "data/DecileData.rds")
