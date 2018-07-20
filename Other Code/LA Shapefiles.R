library(rgdal)
library(sp)
library(rgeos)

shps <- readRDS("C:/Users/cassidy.nicholas/OneDrive - IS/WRDashboard/LAPolys.rds")

shps <- gSimplify(shps, 0.0005,TRUE)

saveRDS(shps, "C:/Users/cassidy.nicholas/OneDrive - IS/CPOP/data/LAShps.rds")
