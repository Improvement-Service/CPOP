library(rgdal)
library(sp)
library(rgeos)

shps <- readRDS("C:/Users/cassidy.nicholas/OneDrive - IS/WRDashboard/LAPolys.rds")

shps2 <- gSimplify(shps, 0.0005,TRUE)
shps <- SpatialPolygonsDataFrame(shps2, shps@data)

saveRDS(shps, "C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/LAShps.rds")
