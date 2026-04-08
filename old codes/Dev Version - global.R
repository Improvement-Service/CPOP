library(shiny)
library(shinydashboard)
library(tidyverse)
library(plyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(Unicode)
library(leaflet)
library(cowplot)


SpPolysDF<-read_rds("Files for Maps/Shapes_decs.rds")
SpPolysIZ <- read_rds("Files for Maps/IZshapes_decs.rds")
CPPdta <- read_csv("CPPcleandata.csv")
IGZdta <- read_csv("IGZcleandata.csv")
IGZ1617 <- read_csv("IGZ1617Typology.csv")
IGZChange <- read_csv("IGZChangeTypology.csv")
CPPdtaCurrent <- read_csv("ImpRateCPP.csv")

##create deciles for colours
CPPMapDta <- SpPolysDF@data
##convert to numeric
CPPMapDta[[15]] <- as.numeric(CPPMapDta[[15]])
CPPMapDta[[14]] <- as.numeric(CPPMapDta[[14]])
