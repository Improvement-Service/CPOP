##Run this code first - it takes all of the prepared data within the 
#spreadsheets
#Then converts these into long format for inclusion in the tool

library(tidyverse)
library(plyr)
library(readxl)

#use for loop to loop through and read every worksheet
##CPP data

CPPdta <- read_excel("data/Final CPP Data - Mar 19.xlsx",
                     sheet = 2)

for (i in 3:20) {
  CPPdta2 <- read_excel("data/Final CPP Data - Mar 19.xlsx",
                        sheet = i)
  CPPdta <- merge(CPPdta, CPPdta2)
}

#combine columns into 1 long variable
CPPdta <- gather(CPPdta, Indicator_Type_Year, value, -1)
#separate 1 column into 3
CPPdta <- separate(CPPdta, Indicator_Type_Year, c("Indicator", "Type", "Year"), sep = "_")
#rename 1st variable
colnames(CPPdta)[1] <- "CPP"

##New column with full indicator names
CPPdta$IndicatorFullName <- CPPdta$Indicator
CPPdta[CPPdta$Indicator == "Child Poverty","IndicatorFullName"] <- "Child Poverty (%)"
CPPdta[CPPdta$Indicator =="Crime Rate","IndicatorFullName"] <- "Crime Rate, per 10,000"
CPPdta[CPPdta$Indicator == "Early Mortality","IndicatorFullName"] <- "Early Mortality, per 100,000"
CPPdta[CPPdta$Indicator =="Emergency Admissions","IndicatorFullName"] <- "Emergency Admissions, per 100,000"
CPPdta[CPPdta$Indicator =="Out of Work Benefits","IndicatorFullName"] <- "Out of Work Benefits (%)"
CPPdta[CPPdta$Indicator =="Positive Destinations","IndicatorFullName"] <- "Positive Destinations (%)"     
CPPdta[CPPdta$Indicator =="Depopulation","IndicatorFullName"] <- "Depopulation Index"
CPPdta[CPPdta$Indicator == "Attainment", "IndicatorFullName"] <- "Average Highest Attainment"
#save CSV data file
write_excel_csv(CPPdta ,path = "data/CPPcleandata.csv")

##IGZ data
IGZdta <- read_excel("data/Final IGZ Data - Mar 19.xlsx",
                     sheet = 2)
for (i in 3:10) {
  IGZdta2 <- read_excel("data/Final IGZ Data - Mar 19.xlsx",
                        sheet = i)
  IGZdta <- merge(IGZdta, IGZdta2)
}

#combine columns into 1 long variable
IGZdta <- gather(IGZdta, Indicator_Type_Year, value, -1,-2,-3,-4,-5)
#separate 1 column into 3
IGZdta <- separate(IGZdta, Indicator_Type_Year, c("Indicator", "Type", "Year"), sep = "_")
##rename indicators- in new column
IGZdta$IndicatorFullName <- IGZdta$Indicator
IGZdta[IGZdta$Indicator == "Child Poverty","IndicatorFullName"] <- "Child Poverty (%)"
IGZdta[IGZdta$Indicator =="Crime Rate","IndicatorFullName"] <- "Crime Rate, per 10,000"
IGZdta[IGZdta$Indicator == "Early Mortality","IndicatorFullName"] <- "Early Mortality, per 100,000"
IGZdta[IGZdta$Indicator =="Emergency Admissions","IndicatorFullName"] <- "Emergency Admissions, per 100,000"
IGZdta[IGZdta$Indicator =="Out of Work Benefits","IndicatorFullName"] <- "Out of Work Benefits (%)"
IGZdta[IGZdta$Indicator =="Positive Destinations","IndicatorFullName"] <- "Positive Destinations (%)"     
IGZdta[IGZdta$Indicator =="Depopulation","IndicatorFullName"] <- "Depopulation Index"
IGZdta[IGZdta$Indicator == "Attainment", "IndicatorFullName"] <- "Average Highest Attainment"
#save CSV data file
write_excel_csv(IGZdta ,path = "data/IGZcleandata.csv")


##DZ data
DZdta <- read_excel("data/Final DZ Data - Mar 19.xlsx",
                     sheet = 2)
for (i in 3:7) {
  DZdta2 <- read_excel("data/Final DZ Data - Mar 19.xlsx",
                        sheet = i)
  DZdta <- merge(DZdta, DZdta2)
}

#combine columns into 1 long variable
DZdta <- gather(DZdta, Indicator_Type_Year, value, -1,-2,-3,-4)
#separate 1 column into 3
DZdta <- separate(DZdta, Indicator_Type_Year, c("Indicator","Type","Year"), sep = "_")
#save CSV data file
write_excel_csv(DZdta ,path = "data/DZcleandata.csv")

