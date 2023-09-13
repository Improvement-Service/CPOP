#------------------Create shapefiles for Cpop maps ---------------
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP")

library(readr)
library(readxl)
library(dplyr)
library(dplyr)
library(sf)
#read shapefiles
SpPolysDF<-read_rds("C:/Users/cassidy.nicholas/OneDrive - IS/Data/Shapefiles/DZ11.rds")

#read higher geography data
dtaGeoHigher <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/HigherGeos2011.xlsx", sheet = 3)[c(1,19,21)]
#merge
SpPolysDF@data <- left_join(SpPolysDF@data, dtaGeoHigher, by = c("DataZone" = "DZ"))
colnames(SpPolysDF@data)[12] <- "council"

##Search for other duplicates
#dups <- unique(SpPolysDF@data[c(7,10)])
#dups <- dups[duplicated(dups$INTZONE_NAME),2]
##Where duplicated add abbreviated Council Name
#SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], abbreviate(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7],6), sep = " ")

#Read the data zone indicator data
indDta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/Final data - Sep 2023/datazone data for maps.xlsx")
SpPolysDF@data <- left_join(SpPolysDF@data, indDta[c(1,3,4,5,6,7,8)], by = c("DataZone" = "Datazone"))
#remove old Intzone name column
SpPolysDF@data$IZname <- SpPolysDF@data$IGZ
SpPolysDF@data <- select(SpPolysDF@data, -IGZ)
#Look for duplicates
dups <- unique(SpPolysDF@data[c(11,12)])
dups <- dups[duplicated(dups$IZname),1]
#Where duplicated add abbreviated Council Name
#SpPolysDF@data[SpPolysDF@data$IZname %in% dups, 11] <- paste(SpPolysDF@data[SpPolysDF@data$IZname %in% dups, 11], SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 12], sep = " ")
#SpPolysDF@data$`Percentage of school leavers entering positive destinations` <- as.numeric(SpPolysDF@data$`Percentage of school leavers entering positive destinations`)

##change council names
SpPolysDF@data[SpPolysDF@data$council == "Na h-Eileanan an Iar",12] <- "Eilean Siar"
SpPolysDF@data[SpPolysDF@data$council == "City of Edinburgh",12] <- "Edinburgh"
dta <- SpPolysDF@data
##read in West Dun and East L. names and replace in all data
wDNms <- read_excel("data/Intermediate Geography Names.xlsx", sheet = 2)
dta <- left_join(dta,wDNms[c(1,3)], by = c("DataZone"))
dta$IZname <- dta$`IZ Name`
dta <- select(dta, -"IZ Name")
rownames(dta) <- 0:6975
SpPolysDF <- st_as_sf(SpPolysDF)
sf_use_s2(FALSE)
SpPolysDF <- st_simplify(SpPolysDF,  dTolerance = 0.5, preserveTopology = TRUE)
#save
saveRDS(SpPolysDF, file = "C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/Shapes2.rds")


## Intermediate Geography Shapes
#Read shapes
SpPolysIZ <- readRDS("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/IZ11.rds")
#Read CPOP data, merge with shapes, clean the ranks into LA "sevenths" (septiles?)
##This is old data, but will be replaced later anyway in the Preprocessing Ranks code
CPdta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/ranks for igzs.xlsx", sheet = 1)
SpPolysIZ@data <- left_join(SpPolysIZ@data, CPdta, by = c("InterZone" = "IGZ code"))
colnames(SpPolysIZ@data)[11] <- "council"
decs <- c()
for(i in unique(SpPolysIZ@data$council)){
  x <- ntile(SpPolysIZ@data[SpPolysIZ@data$council == i, 12], 7)
  decs <- c(decs, x)
}
SpPolysIZ@data$rank_decs <- decs

SpPolysIZ@data[SpPolysIZ@data$council == "Edinburgh, City of", 11] <- "Edinburgh"

SpPolysIZ <- spTransform(SpPolysIZ, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
dta <- SpPolysIZ@data
rownames(dta) <- 0:1278
##simplify to make the size smaller
SpPolysIZ <- st_as_sf(SpPolysIZ)
sf_use_s2(FALSE)
SpPolysIZ <- st_simplify(SpPolysIZ,  dTolerance = 0.5, preserveTopology = TRUE)

#Save 
saveRDS(SpPolysIZ, "C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/IZshapes2.rds")
