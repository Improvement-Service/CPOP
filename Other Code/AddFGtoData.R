###Match family groups to CPPs
library(tidyverse)
library(readxl)

#read in family group data
FGdta <- read_excel("Family Groups.xlsx")
FGdta[FGdta$X__1 == "Edinburgh City",1] <- "Edinburgh, City of"

##read in CPP data
CPPdta <- read_csv("CPPcleandata.csv")

##match all FGs to CPP data
CPPdta <- left_join(CPPdta, FGdta, by = c("CPP" = "X__1"))

##if the indicator is rurality replace the FG
CPPdta$FG <-with(CPPdta, ifelse(Indicator %in% c("Fragility","Fuel Poverty"), popdensityFG_2012, deprivationFG_2012))
CPPdta <- CPPdta[c(1:5,8)]

write_csv(CPPdta, "CPPcleandata.csv")
