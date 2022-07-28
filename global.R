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
library(stringr)

#Store value for the most recent year data is available, this needs to be changed when data is refreshed annually
FrstYear <- "2008/09"
RcntYear <- "2019/20"
ProjYear <- "2022/23"

#First and last years for Duncan Index graphs
DIFrYr <- substr(FrstYear,1,4)
DIRcYr <- substr(RcntYear,1,4)

LblFrst <- "08/09"
LblRcnt <- "19/20"
LblProj <- "22/23"

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

#global variables extracted from server script
latestScotlandDta <- CPP_Imp %>% filter(Year == RcntYear & CPP == "Scotland")
indicators <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
                  "Attainment", "Positive Destinations", "Employment Rate",
                  "Median Earnings", "Out of Work Benefits", "Business Survival",
                  "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
                  "Emergency Admissions", "Unplanned Hospital Attendances",
                  "Early Mortality", "Fragility", "Well-being", "Fuel Poverty")
latest_CPP_Imp <- CPP_Imp %>% filter(Year == RcntYear)

#family groups=========================
#read in family group data
#FGdta <- read_excel("data/Family Groups.xlsx")
#FGdta[FGdta$X__1 == "Edinburgh City",1] <- "Edinburgh, City of"

##match all FGs to CPP data
#CPPdta <- left_join(CPPdta, FGdta, by = c("CPP" = "X__1"))
#CPPdta <- CPPdta[c(1:5,7)]
#colnames(CPPdta)[[6]] <- "FG"

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

plot_with_metadata_pop_up <- function (metadata, plotName, indicatorTitle, plc = "top", plotHeight = "25vh"){
  indicatorMetadata <- filter(metadata, Indicator == indicatorTitle)
  
  column(2, 
         style = paste0("margin-left:0px;margin-right:0px;padding-right:0px; padding-left:0px; height:", plotHeight,"!important"),
         plotOutput(plotName, height= plotHeight),
         bsPopover(id = plotName,
                   title = indicatorTitle, 
                   content = paste(
                     "<b>Definition</b></p><p>",
                     indicatorMetadata[[1,2]],
                     "</p><p>",
                     "<b>Raw Time Period</b></p><p>",
                     indicatorMetadata[[1,3]],
                     "</p><p>",
                     "<b>Source</b></p><p>",
                     indicatorMetadata[[1,4]]
                   ),
                   placement = plc,
                   trigger = "hover",
                   options = list(container = "body")
         )
  )
}

trafficLightMarkerColour <- function (LA_dta, comparator_dta) {
  highIsPositive <- unique(LA_dta$`High is Positive?`)
  
  if_else(last(LA_dta$value) > last(comparator_dta$value), 
          if_else(last(LA_dta$Improvement_Rate) > last(comparator_dta$Improvement_Rate),
                  if_else(highIsPositive == "Yes",
                          "green",
                          "red"),
                  "yellow"),
          if_else(last(LA_dta$value) < last(comparator_dta$value),
                  if_else(last(LA_dta$Improvement_Rate) < last(comparator_dta$Improvement_Rate),
                          if_else(highIsPositive == "Yes",
                                  "red",
                                  "green"),
                          "yellow"),
                  "black")
  )
}

show_DZ_pop_up <- function(group, lat, lng, colIndex, plotId) {
  selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
  setnames(selectedDZ, old = colnames(selectedDZ[13:18]), 
             new = c("Children in Poverty (%)", "Average Highest Attainment", "Positive Destinations (%)", "Out of Work Benefits (%)", "SIMD Crimes per 10,000","Emergency Admissions per 100,000"))
  content <- as.character(tagList(
    tags$h4(as.character(unique(selectedDZ$DataZone))),
    sprintf(
      "%s: %s\n",
      names(selectedDZ[colIndex]), 
      round(unique(as.numeric(selectedDZ[colIndex])),2)
    ), 
    tags$br()
  ))
  leafletProxy(plotId) %>% addPopups(lng, lat, content, layerId = group)
}

add_selected_indicators_tags <- function (dataset, var, input1, input2 = NULL) {
  var <- enquo(var)
  if(is.null(input2)) 
  {
    dta <- dataset %>% 
      mutate(userSelection = ifelse(!!var == input1, "A", "C"))
  }
  else
  {
    dta <- dataset %>% 
      mutate(userSelection = ifelse(!!var == input1, 
                                    "A",
                                    ifelse(!!var == input2, 
                                           "B", 
                                           "C")))
  }
  return(dta)
}
