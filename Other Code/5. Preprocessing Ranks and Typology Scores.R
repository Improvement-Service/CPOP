##File for data prep. 

#RUN AFTER clean data.R and AddFGtoData.R and ShapefileCode.R
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")

library(dplyr)
library(tidyverse)
library(DT)
library(data.table)
library(readxl)
library(sf)

#Store value for the start year and most recent year data is available, this needs to be changed when data is refreshed annually
StrtYear <- "2010/11"
RcntYear <- "2021/22"

SpPolysDF <- read_rds("data/Shapes.rds")
SpPolysIZ <- read_rds("data/IZshapes.rds") %>% select(-`X.1.is.least.deprived.`)
CPPdta <- read_csv("data/CPPcleandata.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")
DZdta <- read_csv("data/DZcleandata.csv")

#read Fife strategic areas for bespoke analysis
fife_sa <- read_excel("data/Fife Strategic Areas.xlsx")

high_is_negative <- c("Dwelling Fires", 
                      "Unplanned Hospital Attendances",
                      "Fuel Poverty", "Fragility", 
                      "Carbon Emissions", "Child Poverty",
                      "Out of Work Benefits", "Crime Rate",
                      "Emergency Admissions",
                      "Early Mortality")

#Calculate percentiles for map colours------------
povDecs <- c()
for(i in unique(SpPolysDF$council)){
  x <- ntile(SpPolysDF[SpPolysDF$council == i, 13], 7)
  povDecs <-c(povDecs,x)
}

tariffDecs <- c()
for(i in unique(SpPolysDF$council)){
  x <- ntile(SpPolysDF[SpPolysDF$council == i, 14], 7)
  tariffDecs <-c(tariffDecs,x)
}

benDecs <- c()
for(i in unique(SpPolysDF$council)){
  x <- ntile(SpPolysDF[SpPolysDF$council == i, 15], 7)
  benDecs <-c(benDecs,x)
}

crimeDecs <- c()
for(i in unique(SpPolysDF$council)){
  x <- ntile(SpPolysDF[SpPolysDF$council == i, 16], 7)
  crimeDecs <-c(crimeDecs,x)
}

admisDecs <- c()
for(i in unique(SpPolysDF$council)){
  x <- ntile(SpPolysDF[SpPolysDF$council == i, 17], 7)
  admisDecs <-c(admisDecs,x)
}

SpPolysDF <- cbind(SpPolysDF, povDecs, tariffDecs,benDecs,crimeDecs, admisDecs)
rm(i, x, povDecs, tariffDecs,benDecs,crimeDecs, admisDecs)


# Create CPP Scores and Type Scores for most recent years data----------
IGZdta <- IGZdta %>% 
  mutate(`High is Positive` = case_when(Indicator %in% high_is_negative ~ "No",
                                         !Indicator %in% high_is_negative ~ "Yes"))


#calculate CPPScore and TypeScore for most recent year
IGZ_latest <- IGZdta %>%
  filter(Year == RcntYear) %>%
  group_by(CPP, Indicator) %>%
  mutate(CPPMean = mean(value)) %>%
  ungroup() %>%
  mutate(Differences = value - CPPMean) %>%
  group_by(CPP, Indicator) %>%
  mutate(StdDev = sd(value)) %>%
  ungroup() %>%
  mutate(ZScore = Differences/StdDev,
         CPPScore = case_when(`High is Positive` == "Yes" ~ ZScore,
                              `High is Positive` == "No" ~ ZScore*-1)) %>%
  select(c(-CPPMean, -Differences, -StdDev, -ZScore)) %>%
  group_by(Typology_Group, Indicator) %>%
  mutate(TypeMean = mean(value)) %>%
  ungroup() %>%
  mutate(Differences = value - TypeMean) %>%
  group_by(Typology_Group, Indicator) %>%
  mutate(StdDev = sd(value)) %>%
  ungroup() %>%
  mutate(ZScore = Differences/StdDev,
         TypeScore = case_when(`High is Positive`  == "Yes" ~ ZScore,
                                `High is Positive`  == "No" ~ ZScore*-1)) %>%
  select(c(-TypeMean, -Differences, -StdDev, -ZScore))

##get latest scores for Fife Strategic Areas================

# CPP Score
IGZ_latest_Fife <- IGZ_latest %>% 
  filter(CPP == "Fife") %>% 
  left_join(fife_sa[c(1,4)], by = c(InterZone = "AreaCode")) %>%
  group_by(`Strategic Area`, Indicator) %>%
  mutate(CPPMean = mean(value)) %>%
  ungroup() %>%
  mutate(Differences = value - CPPMean) %>%
  group_by(`Strategic Area`, Indicator) %>%
  mutate(StdDev = sd(value)) %>%
  ungroup() %>%
  mutate(ZScore = Differences/StdDev,
         CPPScore = case_when(`High is Positive` == "Yes" ~ ZScore,
                              `High is Positive` == "No" ~ ZScore*-1)) %>%
  select(c(-CPPMean, -Differences, -StdDev, -ZScore))


# Create CPP Scores & Typology Scores for the change from start to finish year--------------
IGZ_change <- IGZdta %>%
  ##replace all 0 values with NA
  mutate(across(value, ~na_if(.,0))) %>%
  filter(Year %in% c(StrtYear,RcntYear)) %>%
  group_by(InterZone, Indicator) %>%
  mutate(Change = last(value) / first(value) -1, 
         Change = case_when(`High is Positive` == "Yes" ~ Change,
                            `High is Positive` == "No" ~ Change*-1)) %>%
  filter(Year == RcntYear) %>%
  ungroup() %>%
  group_by(Indicator) %>%
  mutate(OverallMean = mean(Change, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Differences = Change - OverallMean) %>%
  group_by(Indicator) %>%
  mutate(StdDev = sd(Change, na.rm= TRUE))%>%
  ungroup() %>%
  mutate(OverallZScore = Differences/StdDev) %>%
  select(c(-OverallMean, -Differences, -StdDev)) %>%
  group_by(CPP, Indicator) %>%
  mutate(CPPMean = mean(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Differences = OverallZScore - CPPMean) %>%
  group_by(CPP, Indicator) %>%
  mutate(StdDev = sd(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(CPPChangeScore = Differences/StdDev)%>%
  select(c(-CPPMean, -Differences, -StdDev))

# Calculate FIFE SA Change Scores
IGZ_change_Fife <- IGZ_change %>% 
  filter(CPP == "Fife") %>% 
  left_join(fife_sa[c(1,4)], by = c(InterZone = "AreaCode")) %>%
  group_by(`Strategic Area`, Indicator) %>%
  mutate(CPPMean = mean(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Differences = OverallZScore - CPPMean) %>%
  group_by(`Strategic Area`, Indicator) %>%
  mutate(StdDev = sd(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(CPPChangeScore = Differences/StdDev) %>%
  ungroup() %>%
  select(c(-CPPMean, -Differences, -StdDev))

# Calculate Typology Change Score
IGZ_change <-IGZ_change %>%
  group_by(Typology_Group, Indicator) %>%
  mutate(TypeMean = mean(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(Differences = OverallZScore - TypeMean) %>%
  group_by(Typology_Group, Indicator) %>%
  mutate(StdDev = sd(OverallZScore, na.rm= TRUE)) %>%
  ungroup() %>%
  mutate(TypeChangeScore = Differences/StdDev) %>%
  select(c(-TypeMean, -Differences, -StdDev))


# Add Z score column to SpPolysDF to allow ranking in this DataFrame --------------------- 

decs <-IGZ_latest %>%
  group_by(InterZone, CPP) %>%
  summarise(combCPP = sum(CPPScore)) %>%
  group_by(CPP) %>%
  mutate(CPPDec = ntile(combCPP, n = 7),
         CPPRank = frank(combCPP)) %>%
  select(InterZone, CPPDec,CPPRank)

SpPolysIZ <- SpPolysIZ %>% 
  left_join(., decs, by = "InterZone") %>% 
  select(-rank_decs, -`rank.min`)

names(SpPolysIZ)[c(13,14)] <- c("rank_decs", "rank-min")

saveRDS(SpPolysDF, "data/Shapes_decs.rds")
saveRDS(SpPolysIZ, "data/IZshapes_decs.rds")
write_csv(IGZ_change,"data/IGZ_change.csv")
write_csv(IGZ_latest,"data/IGZ_latest.csv")

#tidy and write Fife files
IGZ_latest_Fife <- IGZ_latest_Fife %>% select(c(`Strategic Area`, InterZone, CPPScore, Indicator)) %>% rename(SAScore = CPPScore)
IGZ_change_Fife <- IGZ_change_Fife %>% select(c(`Strategic Area`, InterZone, CPPChangeScore, Indicator)) %>% rename(SAChangeScore = CPPChangeScore)
write_csv(IGZ_latest_Fife,"data/IGZ_latest_Fife.csv")
write_csv(IGZ_change_Fife,"data/IGZ_change_Fife.csv")

# Compute CPP improvement rates and store data for plots on CPP over time page--------


CPP_dta_current <- CPPdta %>% 
  mutate(LineType =  paste(CPP, Type)) %>%
  filter(Type != "Projected") %>%
  group_by(CPP, Indicator) %>%
  mutate(Improvement_Rate = (last(value)/first(value) - 1) * 100)

CPP_dta_current <- setDT(CPP_dta_current)[, Improvement_Rate :=
                                            (last(value) / first(value) -1) * 100,
                                          by = list(CPP, Indicator)
                                          ]

CPP_dta_current <- CPP_dta_current %>% 
  mutate(`High is Positive` = case_when(Indicator %in% high_is_negative ~ "No",
                                         !Indicator %in% high_is_negative ~ "Yes"))

write_csv(CPP_dta_current, "data/Imp_rate_CPP.csv")
