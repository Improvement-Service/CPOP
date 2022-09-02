##File for adding family group info to CPP dta
#RUN AFTER Clean data.R

##After this run ShapefilesCode.R

###Match family groups to CPPs
library(tidyverse)
library(readxl)

#read in family group data
FGdta <- read_excel("data/Family Groups.xlsx")
names(FGdta)[1] <- "CPP"
FGdta[FGdta$CPP == "Edinburgh City",1] <- "Edinburgh, City of"

##read in CPP data
CPPdta <- read_csv("data/CPPcleandata.csv")

##match all FGs to CPP data
CPPdta <- left_join(CPPdta, FGdta, by = c("CPP" = "CPP"))

###if the indicator is rurality replace the FG - decided not to do this
#CPPdta$FG <-with(CPPdta, ifelse(Indicator %in% c("Fragility","Fuel Poverty"), popdensityFG_2012, deprivationFG_2012))
#CPPdta <- CPPdta[c(1:5,8)]
CPPdta$FG <- CPPdta$deprivationFG_2012
CPPdta <- CPPdta[c(1:6,9)]
write_csv(CPPdta, "data/CPPcleandata.csv")
