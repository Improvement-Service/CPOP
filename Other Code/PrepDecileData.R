##prepare inequality data for table
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")
decDta <- read_csv("data/IncomeDeciles.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")

IGZdta <- left_join(IGZdta, decDta, by = c("InterZone" = "AreaCode"))
scotvals <- filter(IGZdta, ScotlandDec %in% c(1,10)) %>%
  group_by(ScotlandDec, Indicator, Year) %>%
  summarise_at(vars(value), funs(mean))
scotvals$CouncilName <- "Scotland"
names(scotvals)[1] <- "Decile"

dd <- IGZdta[c(11,6,8:10)] %>% filter(Decile %in% c(1,10))%>%
  group_by(Decile,Year,CouncilName,Indicator) %>%
  summarise_at(vars(value), funs(mean))
dd <- bind_rows(dd[1:5], scotvals[1:5])

dd <- filter(dd, Year %in% c("2006/07", "2007/08", "2008/09", "2009/10", 
                             "2010/11", "2011/12", "2012/13",
                             "2013/14", "2014/15", "2015/16", "2016/17"))
saveRDS(dd, "data/DecileData.rds")
