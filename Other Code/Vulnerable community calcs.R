library(tidyverse)
library(plyr)

###NO LONGER NEED THIS AS GOING TO SELECT EARLIEST AND MOST RECENT YEAR
#StartYear <- "2006/07"
#EndYear   <- "2017/18"

CPPNames <- c("Aberdeen City", "Aberdeenshire", "Angus", "Argyll and Bute",
              "Clackmannanshire", "Dumfries and Galloway","Dundee City",
              "East Ayrshire", "East Dunbartonshire", "East Lothian",
              "East Renfrewshire", "Edinburgh, City of", "Eilean Siar",
              "Falkirk", "Fife", "Glasgow City", "Highland",
              "Inverclyde", "Midlothian", "North Ayrshire",
              "North Lanarkshire", "Orkney Islands", "Perth and Kinross",
              "Renfrewshire", "Scottish Borders",
              "Shetland Islands", "South Ayrshire", "South Lanarkshire",
              "Stirling", "West Dunbartonshire", "West Lothian")



IGZData <- read_csv("data/IGZcleandata.csv")[c(1:9)]
colnames(IGZData)[1] <- "IGZ"
IGZData <- filter(IGZData, Type == "Raw data") %>% select(-Type)
##get labels for later
Labels <- IGZData[1:2]
IGZData <- IGZData[-2]
#Calculations for table 1 --------------------------------------------------------------------------------
#Calculate Change and CPPChangeScore----------------------------------------------------------------------
IGZData$HighIsPos <- "No"
IGZData$HighIsPos[IGZData$Indicator %in% c("Attainment", "Positive Destinations")] <- "Yes"

#Calculate %Change from First year to last year
TableData <- IGZData %>%
  group_by(Indicator) %>% 
  filter(Year %in% c(first(Year), last(Year))) %>%
  mutate(YearRef = if_else(Year == first(Year),1,2))
TableData <- TableData[order(TableData$YearRef),]
TableData[,7] <- round(TableData[,7],2)

TableData <- ddply(
  TableData,. 
  (IGZ, Indicator),
  transform, 
  Change = ((last(value) / first(value))-1)*100
)

#Calculate the average of this change for each CPP

TableData <- ddply(
  TableData,. 
  (CPP, Indicator, YearRef),
  transform, 
  CPPChangeMean = mean(Change)
)

#Calculate the Standard Deviation of this change for each CPP

TableData <- ddply(
  TableData,. 
  (CPP, Indicator, YearRef),
  transform, 
  CPPChangeSD = sd(Change)
)

#Calculate difference between change value and CPPChangeMean 

TableData$Differences <- TableData$Change - TableData$CPPChangeMean

#Calculate CPPChangeScore (z score - describes how many standard deviations the change in value is from the 
#mean change in value for that CPP)

TableData$CPPChangeScore <- TableData$Differences / TableData$CPPChangeSD

#Remove columns which are not needed, CPPChangeMean, CPPChangeSD & Differences

TableData <- TableData[,-c(11:13)]

#Multiply change values and scores by -1 so that improvement is always measured by a positive value

TableData$CPPChangeScore[TableData$HighIsPos == "No"] <- TableData$CPPChangeScore[TableData$HighIsPos == "No"]*-1

#Calculate combined change and type scores, combining the individual scores for each outcome

TableData <- ddply(
  TableData,. 
  (IGZ, YearRef),
  transform, 
  CombinedCPPChangeScore = sum(CPPChangeScore)
)


#Remove columns which are not needed, CPPChangeScore, TypeChangeScore

TableData <- TableData[,-11]

#Add in CPP average of values to include as bottom row

TableData <- ddply(
  TableData,. 
  (CPP, Indicator, YearRef),
  transform, 
  CPPAverage = mean(value)
)

#Format data table-----------------------------------------------------------------------------------------

#Seperate out change values only and format

ChangeData <- TableData[,c(1,2,3,4,5,6,8,9,10,12)]
ChangeData$Label <- "Change"

#Calculate change over time in CPP average
ChangeData <- ddply(
  ChangeData,. 
  (IGZ, Indicator),
  transform, 
  ChangeAv = ((last(CPPAverage) / first(CPPAverage))-1)*100
)

#Multiply change values and scores by -1 so that improvement is always measured by a positive value

ChangeData$Change[ChangeData$HighIsPos == "No"] <- ChangeData$Change[ChangeData$HighIsPos == "No"]*-1
ChangeData$ChangeAv[ChangeData$HighIsPos == "No"] <- ChangeData$ChangeAv[ChangeData$HighIsPos == "No"]*-1

ChangeData$Rate <- ifelse(
  ChangeData$Change > ChangeData$ChangeAv,
  "Faster",
  ifelse(
    ChangeData$Change < ChangeData$ChangeAv,
    "Slower",
    "No Difference"
  )
)

ChangeData <- ChangeData[,-c(7,8,9,10,12)]

ChangeData <- ChangeData %>% unite(DataSpec, Indicator, Year, Label, sep = "_")

ChangeData <- ChangeData %>% spread(DataSpec, Rate)

#Only want to keep 1 year for each outcome so remove first year values

ChangeData <- ChangeData[,-c(5,7,9,11,13,15,17,19)]

#Seperate out CPPScores only and format

CPPScoreData <- TableData[,c(1,2,3,4,5,6,8,9,11)]
CPPScoreData$Label <- "CPPScore"
CPPScoreData <- CPPScoreData %>% unite(DataSpec, Indicator, Year, Label, sep = "_")
CPPScoreData <- CPPScoreData[,-c(6:7)]
CPPScoreData <- CPPScoreData %>% spread(DataSpec, CombinedCPPChangeScore)

CPPScoreData[,c(5:20)] <- ifelse(
  CPPScoreData[,c(5:20)] > 0,
  "Faster",
  ifelse(
    CPPScoreData[,c(5:20)] == 0,
    "No Difference",
    "Slower"
  )
)

#Only want to keep 1 column, as the score should be the same for every year and every outcome

CPPScoreData <- CPPScoreData[,-c(5:19)]

#Seperate out CPPMean only and format

CPPMeanData <- TableData[,c(1,2,3,4,5,6,8,9,12)]
CPPMeanData$Label <- "CPPMean"
CPPMeanData <- CPPMeanData %>% unite(DataSpec, Indicator, Year, sep = "_")
CPPMeanData <- CPPMeanData[,-c(6:7)]
CPPMeanData <- CPPMeanData %>% spread(DataSpec, CPPAverage)

#Format TableData

TableData <- TableData[,c(1:7)]
TableData$Label <- "Value"
TableData <- TableData %>% unite(DataSpec, Indicator, Year, sep = "_")
TableData <- TableData %>% spread(DataSpec, value)

#Add Vulnerable Community Labels to data and filter to only include these

DeprivedCommData <- read_csv("data/Vulnerable communities - 2006.csv")
DeprivedCommData <- filter(DeprivedCommData, revRank %in% 1:5) %>%
  select(c("InterZone", "revRank"))
colnames(DeprivedCommData) <- c("IGZ","Most_Deprived_Comm")
DeprivedCommData <- DeprivedCommData[c(2,1)]
TableData <- merge(TableData, DeprivedCommData)
TableData <- TableData[order(TableData$Most_Deprived_Comm),]

CPPMeanData <- merge(CPPMeanData, DeprivedCommData)
CPPMeanData <- filter(CPPMeanData, `Most_Deprived_Comm` == 1)

total <- rbind(TableData, CPPMeanData)
total <- merge(total, ChangeData)
total <- merge(total, CPPScoreData)

total <- unique(left_join(total, Labels, by = "IGZ"))
total <- total[,-c(1,3,4)]

#relabel in correct format, format in correct order and round values

total$Most_Deprived_Comm[total$Label == "CPPMean"] <- "6"
total <- total[order(total$Most_Deprived_Comm),]
total$Most_Deprived_Comm[total$Most_Deprived_Comm == 1] <- "1st Most Vulnerable"
total$Most_Deprived_Comm[total$Most_Deprived_Comm == 5] <- "5th Most Vulnerable"
total$AreaLabel <- total$CPP
total$AreaLabel[total$Label == "CPPMean"] <- "CPP Average"
total <- total[,-2]

total <- total[,c(29,1,18,27,4,5,20,6,7,21,8,9,22,10,11,23,12,13,24,14,15,25,16,17,26,2,3,19,28)]


total[,c(5,6,11,12,20,21,23,24,26,27)] <- round(total[,c(5,6,11,12,20,21,23,24,26,27)],2)
total[,c(8,9,14,15,17,18)] <- round(total[,c(8,9,14,15,17,18)],0)
total[total$AreaLabel =="CPP Average","InterZone_Name"] <- paste(total[total$AreaLabel =="CPP Average","CPP"], "Average")
#Save formatted data

write_excel_csv(total, path = "data/Formatted Vulnerable Communities.csv")

