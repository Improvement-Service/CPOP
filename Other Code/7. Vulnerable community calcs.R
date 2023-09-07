##This code creates a tidy table for the "Vulnerable Communities" page
##Note that the IGZs selected as most vulnerable do not change, but they can be updated to reflect
##new data using the "Calculate most vuln communities.R" code

#setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")

library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
#read in data and define global variables ---------------
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

#obtain the 5 most vulnerable communities for each CPP
DeprivedCommData <- read_csv("data/Vulnerable communities - 2006.csv", show_col_types = FALSE) %>%
  filter(revRank %in% 1:5) %>%
  select(c("revRank","InterZone")) %>%
  dplyr::rename(IGZ = InterZone, Most_Deprived_Comm = revRank)

vuln_iz_list <- unique(DeprivedCommData$IGZ)

#get intermediate geography zone data and drop irrelevant columns
IGZData_raw <- read_csv("data/IGZcleandata.csv",show_col_types = FALSE) %>%
  dplyr::rename(IGZ = InterZone)

IGZData <- IGZData_raw %>%
  filter(Type == "Raw data") %>%
  select(-c(Typology_Group, Typology_Name, Type, IndicatorFullName, InterZone_Name))

#retain IGZ names to match by later
igz_names <- IGZData_raw %>% 
  select(IGZ, InterZone_Name)


#Calculate Change and CPPChangeScore----------------------------------------------------------------------

#Calculate % change from base year, CPPAverage for each indicator and CombinedCPPChangeScore 
    #(the sum of z scores for the IZ % change)
percentage_change_data <- IGZData %>%
  group_by(Indicator) %>% 
  filter(Year %in% c(first(Year), last(Year))) %>%
  mutate(HighIsPos = ifelse(Indicator %in% c("Attainment", "Participation Rate"), 
                            "Yes", 
                            "No"),
         YearRef = case_when(
    Year == first(Year) ~1,
    Year == last(Year) ~2
  ),
  value = round(value,2)) %>%
  arrange(YearRef) %>%
  group_by(IGZ, Indicator) %>%
  mutate(Change = case_when(
    HighIsPos == "No" ~((last(value) / first(value))-1)*100*-1,
    HighIsPos == "Yes" ~((last(value) / first(value))-1)*100
  )) %>%
  group_by(CPP, Indicator, YearRef) %>% 
  mutate(CPPChangeMean = mean(Change),#Calculate the average of this change for each CPP
         CPPChangeSD = sd(Change)) %>% #Calculate the Standard Deviation of this change for each CPP
  ungroup() %>%
  #Calculate CPPChangeScore (z score - describes how many standard deviations the change in value is from the mean change in value for that CPP)
  #Multiply change values and scores by -1 so that improvement is always measured by a positive value
  mutate(CPPChangeScore = case_when(
    HighIsPos == "No" ~((Change - CPPChangeMean)/CPPChangeSD)*-1,
    HighIsPos == "Yes" ~(Change - CPPChangeMean)/CPPChangeSD
    ))
TableData <- percentage_change_data %>%
  select(-CPPChangeMean, -CPPChangeSD) %>%
  group_by(IGZ, YearRef) %>%
  #Calculate combined change scores, combining the individual scores for each outcome
  mutate(CombinedCPPChangeScore = sum(CPPChangeScore)) %>%
  select(-CPPChangeScore) %>%
  group_by(CPP, Indicator, YearRef) %>%
  mutate(CPPAverage = mean(value)) %>%#Add in CPP average of values to include as bottom row
  ungroup()

#extract data for alternative viz
outcome_values <- TableData %>%
  select(IGZ, CPP, Indicator, Year, YearRef, value, CPPAverage)
  
#Format data table-----------------------------------------------------------------------------------------

#Separate out change values only and format

ChangeData <- TableData %>% 
  select(-value, -CombinedCPPChangeScore) %>%
  group_by(IGZ, Indicator) %>%
  #Calculate change over time in CPP average
  mutate(ChangeAv = case_when(
    HighIsPos == "No" ~ ((last(CPPAverage) / first(CPPAverage))-1)*100*-1,
    HighIsPos == "Yes" ~ ((last(CPPAverage) / first(CPPAverage))-1)*100,
  ),
  Rate = case_when(
    Change > ChangeAv ~"Faster",
    Change < ChangeAv ~"Slower",
    Change == ChangeAv ~"No Difference"
  ),
  Label = "Change")

change_values <- ChangeData %>%
  select(IGZ, CPP, Indicator, Year, YearRef, Change, ChangeAv)

ChangeData_Text <- ChangeData %>%
  filter(YearRef==2) %>%
  unite(DataSpec, Indicator, Year, Label, sep = "_") %>%
  select(-Change, -ChangeAv, -CPPAverage, -HighIsPos, -YearRef) %>%
  distinct() %>%
  pivot_wider(names_from = DataSpec, values_from = Rate)

#Separate out CPPScores only and format
#retain combined CPP change score
CPPScoreData <- TableData %>%
  select(-c(Change, CPPAverage, HighIsPos, YearRef, value)) %>%
  mutate(Label = "CPPScore") %>%
  unite(DataSpec, Indicator, Year, Label, sep = "_") %>%
  filter(DataSpec == first(DataSpec)) %>%
  mutate(DataSpec = "CPPScore") %>%
  pivot_wider(names_from = DataSpec, values_from = CombinedCPPChangeScore) %>%
  mutate(CPPScore = case_when(
    CPPScore > 0 ~ "Faster",
    CPPScore < 0 ~ "Slower",
    CPPScore == 0 ~ "No Difference"
  ))


#Seperate out CPPMean only and format

CPPMeanData <- TableData %>%
  select(-c(Change, CombinedCPPChangeScore, HighIsPos, YearRef, value)) %>%
  mutate(Label = "CPPMean") %>% 
  unite(DataSpec, Indicator, Year, sep = "_") %>%
  pivot_wider(names_from = DataSpec, values_from = CPPAverage)
CPPMeanData <- left_join(DeprivedCommData, CPPMeanData) %>%
  filter(Most_Deprived_Comm == 1)


#Format TableData
TableData2 <- TableData %>%
  select(-c(HighIsPos, YearRef, Change, CombinedCPPChangeScore, CPPAverage)) %>%
  mutate(Label = "Value")%>% 
  unite(DataSpec, Indicator, Year, sep = "_") %>%
  pivot_wider(names_from = DataSpec, values_from = value)
#Add Vulnerable Community Labels to data and filter to only include these
TableData2 <- left_join(DeprivedCommData, TableData2) %>%
  arrange(Most_Deprived_Comm)

total <- rbind(TableData2, CPPMeanData) %>%
  left_join(ChangeData_Text) %>%
  left_join(CPPScoreData) %>%
  left_join(igz_names, by = "IGZ") %>%
  distinct() %>%
  mutate(Most_Deprived_Comm = case_when(
    Label == "CPPMean" ~ "6",
    Most_Deprived_Comm == 1 ~ "1st Most Vulnerable",
    Most_Deprived_Comm == 5 ~ "5th Most Vulnerable"
    ),
    AreaLabel = case_when(
      Label == "CPPMean" ~ "CPP Average",
      Label != "CPPMean" ~ CPP
    ),
    InterZone_Name = case_when(
      AreaLabel == "CPP Average" ~ paste0(CPP, " Average"),
      AreaLabel != "CPP Average" ~ InterZone_Name
    )) %>%
  select(AreaLabel, CPP, InterZone_Name, Most_Deprived_Comm, CPPScore, `Child Poverty_2014/15`,
         `Child Poverty_2020/21`, `Child Poverty_2020/21_Change`, `Crime Rate_2009/10`,
         `Crime Rate_2020/21`, `Crime Rate_2020/21_Change`, `Depopulation_2009/10`,
         `Depopulation_2020/21`, `Depopulation_2020/21_Change`, `Early Mortality_2010/11`,
         `Early Mortality_2020/21`, `Early Mortality_2020/21_Change`, `Emergency Admissions_2009/10`,
         `Emergency Admissions_2020/21`, `Emergency Admissions_2020/21_Change`, `Out of Work Benefits_2012/13`,
         `Out of Work Benefits_2020/21`, `Out of Work Benefits_2020/21_Change`, `Participation Rate_2015/16`,
         `Participation Rate_2020/21`, `Participation Rate_2020/21_Change`, `Attainment_2013/14`,
         `Attainment_2020/21`, `Attainment_2020/21_Change`)

whole_num_cols <- c("Crime Rate_2009/10","Crime Rate_2020/21",  
                    "Early Mortality_2010/11","Early Mortality_2020/21", 
                    "Emergency Admissions_2009/10","Emergency Admissions_2020/21")
total[,whole_num_cols] <- round(total[,whole_num_cols], 0)
total <- lapply(total, function(x) if(is.numeric(x)) round(x, 1) else x) %>%
  as.data.frame()

#data formatting for new vizualisations -----------

new_viz_data <- outcome_values %>%
  left_join(change_values)

new_viz_vuln <- DeprivedCommData %>% 
  left_join(new_viz_data) %>%
  rename(vulnerability_rank = Most_Deprived_Comm) %>%
  left_join(igz_names) %>%
  distinct()

inds <- unique(new_viz_vuln$Indicator)
ind_col <- c()
 for (i in inds) {
   ind_col <- c(ind_col, rep(i, 32))
 }

spacer_values <- as.data.frame(matrix(c(
  rep(6,256),
  rep(NA, 256),
  rep(unique(new_viz_data$CPP), 8),
  ind_col,
  rep(NA, 256),
  rep(1, 256),
  rep(NA, 256),
  rep(NA, 256),
  rep(NA, 256),
  rep(NA, 256),
  rep(" ", 256)
),
nrow = 256,
byrow = FALSE,
dimnames = list(c(), names(new_viz_vuln))))

final_new_viz <- rbind(new_viz_vuln,
                       spacer_values) %>%
  arrange(CPP, Indicator, vulnerability_rank) %>%
  mutate(group = paste0(Indicator, " ", InterZone_Name),
         vulnerability_rank = as.numeric(vulnerability_rank),
         label = case_when(
           vulnerability_rank == 3~Indicator,
           vulnerability_rank != 3~" ",
         ),
         Change = case_when(
           Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(Change), 0),
           !Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(Change), 2)
           ),
         ChangeAv = case_when(
           Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(ChangeAv), 0),
           !Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(ChangeAv), 2)
         ),
         CPPAverage = case_when(
           Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(CPPAverage), 0),
           !Indicator %in% c("Crime Rate", "Early Mortality", "Emergency Admissions") ~round(as.numeric(CPPAverage), 2)
         )) 

vul_com_outcomes <- final_new_viz %>%
  mutate(YearRef = case_when(
    YearRef == 1 ~ "BaseYear",
    YearRef == 2 ~ "RecentYear"
  )) %>%
  rename("CommunityValue" = "value") %>%
  pivot_wider(names_from = YearRef, values_from = c(CommunityValue, CPPAverage)) %>%
  fill(c(CommunityValue_BaseYear, CPPAverage_BaseYear), .direction = "down") %>%
  fill(c(CommunityValue_RecentYear, CPPAverage_RecentYear), .direction = "up") %>%
  filter(is.na(Year) | Year != "2020/21") %>%
  rename("BaseYear" = "Year") %>%
  arrange(vulnerability_rank)

vul_com_outcomes$CommunityValue_RecentYear[vul_com_outcomes$vulnerability_rank == 6] <- vul_com_outcomes$CommunityValue_RecentYear[vul_com_outcomes$vulnerability_rank == 5]
vul_com_outcomes$CPPAverage_RecentYear[vul_com_outcomes$vulnerability_rank == 6] <- vul_com_outcomes$CPPAverage_RecentYear[vul_com_outcomes$vulnerability_rank == 5]
vul_com_outcomes$BaseYear[vul_com_outcomes$vulnerability_rank ==6] <- vul_com_outcomes$BaseYear[vul_com_outcomes$vulnerability_rank ==5]
vul_com_outcomes$InterZone_Name[vul_com_outcomes$vulnerability_rank ==6] <- vul_com_outcomes$CPP[vul_com_outcomes$vulnerability_rank ==6]




#Save formatted data
write.csv(total, file = "data/Formatted Vulnerable Communities 2.csv", row.names = FALSE)
write.csv(vul_com_outcomes, file = "data/vulnerable_communities_outcomes_and_change.csv", row.names = FALSE)
