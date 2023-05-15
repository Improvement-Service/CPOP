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
library(shinyBS)
library(shinycssloaders)
library(shinyLP)
library(kableExtra)
library(shinyjs)
library(shinyWidgets)
library(formattable)
library(shinyalert)



#Store value for the most recent year data is available, this needs to be changed when data is refreshed annually
FrstYear <- "2009/10"
RcntYear <- "2020/21"
ProjYear <- "2023/24"

#First and last years for Duncan Index graphs
DIFrYr <- substr(FrstYear,1,4)
DIRcYr <- substr(RcntYear,1,4)

LblFrst <- "09/10"
LblRcnt <- "20/21"
LblProj <- "23/24"

SpPolysDF <- read_rds("data/Shapes_decs.rds")
SpPolysIZ <- read_rds("data/IZshapes_decs.rds")
SpPolysLA <- read_rds("data/LAShps.rds")
CPPdta <- read_csv("data/CPPcleandata.csv")
CPP_Imp <- read_csv("data/Imp_rate_CPP.csv")
IGZdta <- read_csv("data/IGZcleandata.csv")
IGZ_latest <- read_csv("data/IGZ_latest.csv")
IGZ_change <- read_csv("data/IGZ_change.csv")
Metadata <- read_csv("data/Metadata.csv")
VulnComm <- read_csv("data/Formatted Vulnerable Communities.csv")

VulnComm$Most_Deprived_Comm[VulnComm$Most_Deprived_Comm == 6] <- ""
VulnComm[VulnComm$AreaLabel == "CPP Average", c(4,7,10,13,16,19,22,25,28)] <- ""
VulnComm <- as.data.frame(VulnComm)

#rename Edinburgh
SpPolysIZ@data[SpPolysIZ@data$council == "Edinburgh","council"] <- "Edinburgh, City of" 
SpPolysDF@data[SpPolysDF@data$council == "Edinburgh","council"] <- "Edinburgh, City of" 

##create deciles for colours
CPPMapDta <- SpPolysDF@data
##convert to numeric
CPPMapDta[[15]] <- as.numeric(CPPMapDta[[15]])
CPPMapDta[[14]] <- as.numeric(CPPMapDta[[14]])


##read in Fife data for MyCommunity
IGZ_latest_Fife <- read_csv("data/IGZ_latest_Fife.csv")
IGZ_change_Fife <- read_csv("data/IGZ_change_Fife.csv")

#global variables (taken from server)------------
#list of indicators
indicators_1 <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
                  "Attainment", "Positive Destinations", "Employment Rate",
                  "Median Earnings", "Out of Work Benefits", "Business Survival",
                  "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
                  "Emergency Admissions", "Unplanned Hospital Attendances",
                  "Early Mortality", "Fragility", "Well-being", "Fuel Poverty"
)

indis <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
           "Attainment", "Positive Destinations", "Employment Rate",
           "Median Earnings", "Out of Work Benefits", "Business Survival",
           "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
           "Emergency Admissions", "Unplanned Hospital Attendances",
           "Early Mortality", "Fragility", "Well-being", "Fuel Poverty")

# "Map2" colours
clrs      <- brewer.pal(7, "RdYlGn")
clrsCB    <- rev(brewer.pal(7, "YlGnBu"))
povPal    <- colorBin(rev(clrs), SpPolysDF@data$povDecs)
povPalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$povDecs)
tariffPal <- colorBin(clrs, SpPolysDF@data$tariffDecs)
tariffPalCB <- colorBin(clrsCB, SpPolysDF@data$tariffDecs)
posPal    <- colorBin(clrs, SpPolysDF@data$posDecs)
posPalCB  <- colorBin(clrsCB, SpPolysDF@data$posDecs)
benPal    <- colorBin(rev(clrs), SpPolysDF@data$benDecs)
benPalCB    <- colorBin(rev(clrsCB), SpPolysDF@data$benDecs)
crimePal  <- colorBin(rev(clrs), SpPolysDF@data$crimeDecs)
crimePalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$crimeDecs)
admisPal  <- colorBin(rev(clrs), SpPolysDF@data$admisDecs)
admisPalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$admisDecs)

# Colours for Community Map
communityPal <- colorBin(clrs, SpPolysIZ@data$rank_decs)
communityPalCB <- colorBin(clrsCB, SpPolysIZ@data$rank_decs)


#family groups=========================
#read in family group data
#FGdta <- read_excel("data/Family Groups.xlsx")
#FGdta[FGdta$X__1 == "Edinburgh City",1] <- "Edinburgh, City of"

##match all FGs to CPP data
#CPPdta <- left_join(CPPdta, FGdta, by = c("CPP" = "X__1"))
#CPPdta <- CPPdta[c(1:5,7)]
#colnames(CPPdta)[[6]] <- "FG"

# Create Methodology Popup Notes ---------------------------------------------------------------------

HealthyBW <- filter(Metadata, Indicator == "Healthy Birthweight")
DefHBW    <- HealthyBW[[1,2]]
TimeHBW   <- HealthyBW[[1,3]]
SourceHBW <- HealthyBW[[1,4]]

BMI       <- filter(Metadata, Indicator == "Primary 1 Body Mass Index")
DefBMI    <- BMI[[1,2]]
TimeBMI   <- BMI[[1,3]]
SourceBMI <- BMI[[1,4]]

CPov       <- filter(Metadata, Indicator == "Child Poverty")
DefCPov    <- CPov[[1,2]]
TimeCPov   <- CPov[[1,3]]
SourceCPov <- CPov[[1,4]]

S4T       <- filter(Metadata, Indicator == "Average Highest Attainment")
DefS4T    <- S4T[[1,2]]
TimeS4T   <- S4T[[1,3]]
SourceS4T <- S4T[[1,4]]

PosDes       <- filter(Metadata, Indicator == "Positive Destinations")
DefPosDes    <- PosDes[[1,2]]
TimePosDes   <- PosDes[[1,3]]
SourcePosDes <- PosDes[[1,4]]

EmpRt       <- filter(Metadata, Indicator == "Employment Rate")
DefEmpRt    <- EmpRt[[1,2]]
TimeEmpRt   <- EmpRt[[1,3]]
SourceEmpRt <- EmpRt[[1,4]]

MedEarn       <- filter(Metadata, Indicator == "Median Earnings")
DefMedEarn    <- MedEarn[[1,2]]
TimeMedEarn   <- MedEarn[[1,3]]
SourceMedEarn <- MedEarn[[1,4]]

OWB       <- filter(Metadata, Indicator == "Out of Work Benefits")
DefOWB    <- OWB[[1,2]]
TimeOWB   <- OWB[[1,3]]
SourceOWB <- OWB[[1,4]]

BusSurv       <- filter(Metadata, Indicator == "Business Survival")
DefBusSurv    <- BusSurv[[1,2]]
TimeBusSurv   <- BusSurv[[1,3]]
SourceBusSurv <- BusSurv[[1,4]]

Crime       <- filter(Metadata, Indicator == "Crime Rate")
DefCrime    <- Crime[[1,2]]
TimeCrime   <- Crime[[1,3]]
SourceCrime <- Crime[[1,4]]

Fire       <- filter(Metadata, Indicator == "Dwelling Fires")
DefFire    <- Fire[[1,2]]
TimeFire   <- Fire[[1,3]]
SourceFire <- Fire[[1,4]]

Emiss       <- filter(Metadata, Indicator == "Carbon Emissions")
DefEmiss    <- Emiss[[1,2]]
TimeEmiss   <- Emiss[[1,3]]
SourceEmiss <- Emiss[[1,4]]

EmAd       <- filter(Metadata, Indicator == "Emergency Admissions")
DefEmAd    <- EmAd[[1,2]]
TimeEmAd   <- EmAd[[1,3]]
SourceEmAd <- EmAd[[1,4]]

HospAtt       <- filter(Metadata, Indicator == "Unplanned Hospital Attendances")
DefHospAtt    <- HospAtt[[1,2]]
TimeHospAtt   <- HospAtt[[1,3]]
SourceHospAtt <- HospAtt[[1,4]]

Mort       <- filter(Metadata, Indicator == "Early Mortality")
DefMort    <- Mort[[1,2]]
TimeMort   <- Mort[[1,3]]
SourceMort <- Mort[[1,4]]

Frag       <- filter(Metadata, Indicator == "Fragility")
DefFrag    <- Frag[[1,2]]
TimeFrag   <- Frag[[1,3]]
SourceFrag <- Frag[[1,4]]

WellB       <- filter(Metadata, Indicator == "Well-being")
DefWellB    <- WellB[[1,2]]
TimeWellB   <- WellB[[1,3]]
SourceWellB <- WellB[[1,4]]

FuelPov       <- filter(Metadata, Indicator == "Fuel Poverty")
DefFuelPov    <- FuelPov[[1,2]]
TimeFuelPov   <- FuelPov[[1,3]]
SourceFuelPov <- FuelPov[[1,4]]

#Create list of CPP names for use in UI
CPPNames <- unique(CPPMapDta[CPPMapDta$council != "Scotland", "council"])


##Read in Duncan Index Scores and calculate whether improving
DIdta <- read_csv("data/DuncanIndex.csv")
DIdta <- DIdta[,-5]
DIdta <- gather(DIdta, "ind", "value",3:9) 
DIdta <- na.omit(DIdta)
DIdta <- setDT(DIdta)[, ImprovementRate :=
                                  (abs(last(value))/abs(first(value)))-1,
                                by = list(la, ind)
                                ]
InqDta <-readRDS("data/DecileData.rds")

popOvs <- function(pltnm,Title,Def,Tm,Src, plc = "top", pltHght = "25vh"){
  column(2, style = paste0("margin-left:0px;margin-right:0px;padding-right:0px; padding-left:0px; height:", pltHght,"!important"), plotOutput(pltnm, height= pltHght),
         bsPopover(id = pltnm,
                   title = Title, 
                   content = paste(
                     "<b>Definition</b></p><p>",
                     Def,
                     "</p><p>",
                     "<b>Raw Time Period</b></p><p>",
                     Tm,
                     "</p><p>",
                     "<b>Source</b></p><p>",
                     Src
                   ),
                   placement = plc,
                   trigger = "click",
                   options = list(container = "body")
         )
  )
}

