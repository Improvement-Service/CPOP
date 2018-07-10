library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(readxl)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(Unicode)
library(leaflet)
library(cowplot)


SpPolysDF <- read_rds("data/Shapes_decs.rds")
SpPolysIZ <- read_rds("data/IZshapes_decs.rds")
CPPdta <- read_csv("data/CPPcleandata.csv")
CPP_Imp <- read_csv("data/Imp_rate_CPP.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")
IGZ_latest <- read_csv("data/IGZ_latest.csv")
IGZ_change <- read_csv("data/IGZ_change.csv")

#rename Edinburgh
SpPolysIZ@data[SpPolysIZ@data$council == "Edinburgh","council"] <- "Edinburgh, City of" 
SpPolysDF@data[SpPolysDF@data$council == "Edinburgh","council"] <- "Edinburgh, City of" 

##create deciles for colours
CPPMapDta <- SpPolysDF@data
##convert to numeric
CPPMapDta[[15]] <- as.numeric(CPPMapDta[[15]])
CPPMapDta[[14]] <- as.numeric(CPPMapDta[[14]])

#family groups=========================
#read in family group data
#FGdta <- read_excel("data/Family Groups.xlsx")
#FGdta[FGdta$X__1 == "Edinburgh City",1] <- "Edinburgh, City of"

##match all FGs to CPP data
#CPPdta <- left_join(CPPdta, FGdta, by = c("CPP" = "X__1"))
#CPPdta <- CPPdta[c(1:5,7)]
#colnames(CPPdta)[[6]] <- "FG"

