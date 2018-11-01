library(tidyverse)
library(plyr)
library(DT)
library(data.table)



#Store value for the start year and most recent year data is available, this needs to be changed when data is refreshed annually
StrtYear <- "2006/07"
RcntYear <- "2016/17"

SpPolysDF <- read_rds("data/Shapes.rds")
SpPolysIZ <- read_rds("data/IZshapes.rds")
CPPdta <- read_csv("data/CPPcleandata.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")
DZdta <- read_csv("data/DZcleandata.csv")


#Calculate percentiles for map colours------------


povDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 13], 7)
  povDecs <-c(povDecs,x)
}

tariffDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 14], 7)
  tariffDecs <-c(tariffDecs,x)
}

posDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 15], 7)
  posDecs <-c(posDecs,x)
}
t <- SpPolysDF@data$`Percentage of school leavers entering positive destinations`==100
posDecs[t] <- 7

benDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 16], 7)
  benDecs <-c(benDecs,x)
}

crimeDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 17], 7)
  crimeDecs <-c(crimeDecs,x)
}

admisDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 18], 7)
  admisDecs <-c(admisDecs,x)
}

SpPolysDF@data <- cbind(SpPolysDF@data, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)
rm(i, x, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)


# Create CPP Scores and Type Scores for most recent years data----------


IGZdta <- IGZdta %>% mutate(`High is Positive?` = "Yes")
IGZdta$`High is Positive?`[IGZdta$Indicator %in% c("Child Poverty","Out of Work Benefits", 
                                                   "Crime Rate", "Emergency Admissions", 
                                                   "Early Mortality", "Depopulation")] <- "No"


IGZ_latest <- filter(IGZdta, Year == RcntYear)

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

IGZ_latest$CPPScore[IGZ_latest$High.is.Positive. == "No"] <- 
  (IGZ_latest$CPPScore[IGZ_latest$High.is.Positive. =="No"]) * -1

IGZ_latest <- select(IGZ_latest, c(-CPPMean, -Differences, -StdDev, -ZScore))

# Type Score

IGZ_latest <- ddply(
  IGZ_latest,. 
  (Typology_Group, Indicator), 
  transform, 
  TypeMean = (mean(value))
  )

IGZ_latest$Differences <- IGZ_latest$value - IGZ_latest$TypeMean

IGZ_latest <- ddply(
  IGZ_latest,. 
  (Typology_Group, Indicator), 
  transform, 
  StdDev = (sd(value))
  )

IGZ_latest$ZScore <- IGZ_latest$Differences / IGZ_latest$StdDev

# If high is bad multiply Z score by minus 1 to ensure direction is the same for all indicators

IGZ_latest$TypeScore <- IGZ_latest$ZScore

IGZ_latest$TypeScore[IGZ_latest$High.is.Positive. =="No"] <-
  (IGZ_latest$TypeScore[IGZ_latest$High.is.Positive. =="No"]) * -1

IGZ_latest <- select(IGZ_latest, c(-TypeMean, -Differences, -StdDev, -ZScore))


# Create CPP Scores & Typology Scores for the change from start to finish year--------------

# calculate overall zscore for change

IGZ_change <- filter(IGZdta, Year %in% c(StrtYear,RcntYear))

# need to group by CPP + IGZ here to stop IGZ's with the same name getting included in the wrong group

IGZ_change <- ddply(
  IGZ_change,. 
  (InterZone, CPP, Indicator), 
  transform, 
  Change = (last(value) / first(value) -1)
  )

# If high is bad multiply Z score by minus 1 to ensure direction is the same for all indicators

IGZ_change$Change[IGZ_change$High.is.Positive. == "No"] <- 
  (IGZ_change$Change[IGZ_change$High.is.Positive. == "No"]) * -1

# Filtering data so that change value is only included once per IGZ

IGZ_change <- filter(IGZ_change, Year == RcntYear)

IGZ_change <- ddply(
  IGZ_change,. 
  (Indicator), 
  transform, 
  OverallMean = (mean(Change))
  )

IGZ_change$Differences <- IGZ_change$Change - IGZ_change$OverallMean

IGZ_change <- ddply(
  IGZ_change,. 
  (Indicator), 
  transform, 
  StdDev = (sd(Change))
  )

IGZ_change$OverallZScore <- IGZ_change$Differences / IGZ_change$StdDev
IGZ_change <- select(IGZ_change, c(-OverallMean, -Differences, -StdDev))

# Calculate CPP Change Score

IGZ_change <- ddply(
  IGZ_change,. 
  (CPP, Indicator), 
  transform, 
  CPPMean = (mean(OverallZScore))
  )

IGZ_change$Differences <- IGZ_change$OverallZScore - IGZ_change$CPPMean

IGZ_change <- ddply(
  IGZ_change,. 
  (CPP, Indicator), 
  transform, 
  StdDev = (sd(OverallZScore))
  )

IGZ_change$CPPChangeScore <- IGZ_change$Differences / IGZ_change$StdDev
IGZ_change <- select(IGZ_change, c(-CPPMean, -Differences, -StdDev))

# Calculate Typology Change Score

IGZ_change <- ddply(
  IGZ_change,. 
  (Typology_Group, Indicator), 
  transform, 
  TypeMean = (mean(OverallZScore))
  )

IGZ_change$Differences <- IGZ_change$OverallZScore - IGZ_change$TypeMean

IGZ_change <- ddply(
  IGZ_change,. 
  (Typology_Group, Indicator), 
  transform, 
  StdDev = (sd(OverallZScore))
  )

IGZ_change$TypeChangeScore <- IGZ_change$Differences / IGZ_change$StdDev
IGZ_change <- select(IGZ_change, c(-TypeMean, -Differences, -StdDev))


# Add Z score column to SpDF to allow ranking in this DataFrame --------------------- 


decs <- c()

# need to group by CPP + IGZ here to stop IGZ's with the same name getting included in the wrong group

decs <- ddply(IGZ_latest,.(InterZone, CPP), summarise, combCPP = sum(CPPScore)) %>%
  ddply(., .(CPP), mutate, CPPDec = ntile(combCPP, n = 7)) %>% 
  ddply(.,.(CPP), mutate, CPPRank = frank(combCPP)) %>% 
  select(InterZone, CPPDec,CPPRank)

SpPolysIZ@data <- left_join(SpPolysIZ@data, decs, by = "InterZone") %>% select(-rank_decs, -`rank-min`)
names(SpPolysIZ@data)[c(13,14)] <- c("rank_decs", "rank-min")

saveRDS(SpPolysDF, "data/Shapes_decs.rds")
saveRDS(SpPolysIZ, "data/IZshapes_decs.rds")
write_csv(IGZ_change,"data/IGZ_change.csv")
write_csv(IGZ_latest,"data/IGZ_latest.csv")


# Compute CPP improvement rates and store data for plots on CPP over time page--------


CPP_dta_current <- CPPdta %>% 
  mutate(LineType =  paste(CPP, Type)) %>%
  filter(Type != "Projected")

CPP_dta_current <- setDT(CPP_dta_current)[, Improvement_Rate :=
                                            (last(value) / first(value) -1) * 100,
                                          by = list(CPP, Indicator)
                                          ]

CPP_dta_current <- CPP_dta_current %>% mutate(`High is Positive?` = "Yes")
CPP_dta_current$`High is Positive?`[CPP_dta_current$Indicator %in% c("Dwelling Fires", 
                                                                     "Unplanned Hospital Attendances",
                                                                     "Fuel Poverty", "Fragility", 
                                                                     "Carbon Emissions", "Child Poverty",
                                                                     "Out of Work Benefits", "Crime Rate",
                                                                     "Emergency Admissions",
                                                                     "Early Mortality")
                                    ] <- "No"

write_csv(CPP_dta_current, "data/Imp_rate_CPP.csv")
