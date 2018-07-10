library(tidyverse)
library(plyr)
library(readxl)

#use for loop to loop through and read every worksheet
##CPP data

CPPdta <- read_excel("Final CPP Data.xlsx",
                     sheet = 2)

for (i in 3:19) {
  CPPdta2 <- read_excel("Final CPP Data.xlsx",
                        sheet = i)
  CPPdta <- merge(CPPdta, CPPdta2)
}

#combine columns into 1 long variable
CPPdta <- gather(CPPdta, Indicator_Type_Year, value, -1)
#seperate 1 column into 3
CPPdta <- separate(CPPdta, Indicator_Type_Year, c("Indicator", "Type", "Year"), sep = "_")
#rename 1st variable
colnames(CPPdta)[1] <- "CPP"
#save CSV data file
write_excel_csv(CPPdta ,path = "CPPcleandata.csv")

##IGZ data
IGZdta <- read_excel("Final IGZ Data.xlsx",
                     sheet = 2)
for (i in 3:10) {
  IGZdta2 <- read_excel("Final IGZ Data.xlsx",
                        sheet = i)
  IGZdta <- merge(IGZdta, IGZdta2)
}

#combine columns into 1 long variable
IGZdta <- gather(IGZdta, Indicator_Type_Year, value, -1,-2,-3,-4,-5)
#seperate 1 column into 3
IGZdta <- separate(IGZdta, Indicator_Type_Year, c("Indicator", "Type", "Year"), sep = "_")
#save CSV data file
write_excel_csv(IGZdta ,path = "IGZcleandata.csv")


##DZ data
DZdta <- read_excel("Final DZ Data.xlsx",
                     sheet = 2)
for (i in 3:7) {
  DZdta2 <- read_excel("Final DZ Data.xlsx",
                        sheet = i)
  DZdta <- merge(DZdta, DZdta2)
}

#combine columns into 1 long variable
DZdta <- gather(DZdta, Indicator_Type_Year, value, -1,-2,-3,-4)
#seperate 1 column into 3
DZdta <- separate(DZdta, Indicator_Type_Year, c("Indicator","Type","Year"), sep = "_")
#save CSV data file
write_excel_csv(DZdta ,path = "DZcleandata.csv")

