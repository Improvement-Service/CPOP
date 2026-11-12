#------------------Create shapefiles for Cpop maps ---------------
setwd("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP")

library(readr)
library(readxl)
library(dplyr)
library(dplyr)
library(sf)
library(rmapshaper)
#read shapefiles
SpPolysDF<-read_rds("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/2026 New Geographies/DZ_Shapes22.rds")

#read higher geography data
dtaGeoHigher <- read_excel("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/2026 New Geographies/Intermediate Geography Names.xlsx")[c(1,2,3,4)]
#merge
SpPolysDF <- left_join(SpPolysDF, dtaGeoHigher, by = c("Datazone" = "DataZone"))
colnames(SpPolysDF)[5] <- "council"

#Read the data zone indicator data
indDta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/Final Data - Oct 2026/datazone data for maps.xlsx")
SpPolysDF <- left_join(SpPolysDF, indDta[c(1,3,4,5,6,7,8)], by = c("Datazone"))

##change council names
SpPolysDF[SpPolysDF$council == "Na h-Eileanan Siar","council"] <- "Eilean Siar"
SpPolysDF[SpPolysDF$council == "City of Edinburgh","council"] <- "Edinburgh"
##capture column order
nm_order <- names(SpPolysDF)
SpPolysDF <- ms_simplify(SpPolysDF, keep = 0.15)
SpPolysDF <- SpPolysDF %>% select(all_of(nm_order))
SpPolysDF <-SpPolysDF %>% rename(DataZone = Datazone)
#save
saveRDS(SpPolysDF, file = "C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/Shapes.rds")


## Intermediate Geography Shapes
#Read shapes
SpPolysIZ <- readRDS("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/2026 New Geographies/IZ_Shapes22.rds")
#Read in names and council
CPdta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/2026 New Geographies/ranks for igzs.xlsx", sheet = 1)
SpPolysIZ <- left_join(SpPolysIZ, CPdta, by = c("Interzone" = "IGZ code"))
colnames(SpPolysIZ)[4] <- "council"
colnames(SpPolysIZ)[1] <- "InterZone"

SpPolysIZ[SpPolysIZ$council == "City of Edinburgh", "council"] <- "Edinburgh"
SpPolysIZ[SpPolysIZ$council == "Na h-Eileanan Siar","council"] <- "Eilean Siar"

#Save 
saveRDS(SpPolysIZ, "C:/Users/cassidy.nicholas/OneDrive - Improvement Service Company/CPOP/data/IZshapes.rds")
