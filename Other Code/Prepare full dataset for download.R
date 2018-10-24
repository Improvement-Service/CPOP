##read in the full datasets that we will spread
CPPdta <- read_csv("data/CPPcleandata.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")


#Data for download
dlData <- CPPdta[c(1,2,4,5)] %>% unite(temp, Indicator, Year, sep = "_") %>% spread(temp, value)
dlIZDta <- IGZdta[c(1,2,3,6,8,9)] %>% unite(temp, Indicator,Year, sep = "_") %>% spread(temp, value)