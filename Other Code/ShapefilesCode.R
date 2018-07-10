#------------------Create shapefiles for Cpop maps ---------------
library(readr)
library(readxl)
library(dplyr)
library(rgdal)
library(dplyr)
library(rgeos)
#read shapefiles
SpPolysDF<-read_rds("C:/Users/cassidy.nicholas/OneDrive - IS/Data/Shapefiles/DZ11.rds")

#read higher geography data
dtaGeoHigher <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/HigherGeos2011.xlsx", sheet = 3)[c(1,19,21)]
#merge
SpPolysDF@data <- left_join(SpPolysDF@data, dtaGeoHigher, by = c("DataZone" = "DZ"))
colnames(SpPolysDF@data)[12] <- "council"

##Search for other duplicates
#dups <- unique(SpPolysDF@data[c(7,10)])
#dups <- dups[duplicated(dups$INTZONE_NAME),2]
##Where duplicated add abbreviated Council Name
#SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], abbreviate(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7],6), sep = " ")

#Read the data zone indicator data
indDta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/datazone data for maps.xlsx")
SpPolysDF@data <- left_join(SpPolysDF@data, indDta[c(1,3,4,5,6,7,8,9)], by = c("DataZone" = "Datazone"))
#remove old Intzone name column
SpPolysDF@data$IZname <- SpPolysDF@data$IGZ
SpPolysDF@data <- select(SpPolysDF@data, -IGZ)
#If IZs begin with IZ then add Council name to the end
SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10] <- paste(SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10], SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 7], sep = " ")
#Look for duplicates
dups <- unique(SpPolysDF@data[c(11,12)])
dups <- dups[duplicated(dups$INTZONE_NAME),2]
#Where duplicated add abbreviated Council Name
SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7], sep = " ")
SpPolysDF@data$`S4 Average tariff score` <- as.numeric(SpPolysDF@data$`S4 Average tariff score`)
SpPolysDF@data$`Percentage of school leavers entering positive destinations` <- as.numeric(SpPolysDF@data$`Percentage of school leavers entering positive destinations`)

##change council names
SpPolysDF@data[SpPolysDF@data$council == "Na h-Eileanan an Iar",12] <- "Eilean Siar"
SpPolysDF@data[SpPolysDF@data$council == "City of Edinburgh",12] <- "Edinburgh"
dta <- SpPolysDF@data
rownames(dta) <- 0:6975
SpPolysDF <- gSimplify(SpPolysDF,  0.00005,TRUE)
SpPolysDF <- SpatialPolygonsDataFrame(SpPolysDF, dta)
#save
saveRDS(SpPolysDF, file = "C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/Shapes.rds")


## Intermediate Geography Shapes
#Read shapes
SpPolysIZ <- readRDS("C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/IZ11.rds")
#Read CPOP data, merge with shapes, clean the ranks into LA "sevenths" (septiles?)
CPdta <- read_excel("C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/ranks for igzs.xlsx", sheet = 1)
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
SpPolysIZ <- gSimplify(SpPolysIZ, 0.00005,TRUE)
SpPolysIZ <- SpatialPolygonsDataFrame(SpPolysIZ, dta)

#Save 
saveRDS(SpPolysIZ, "C:/Users/cassidy.nicholas/OneDrive - IS/CMaps - 2011/IZshapes.rds")
