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
library(shinyalert, quietly = TRUE)
library(plotly)
library(sf)
library(ggbump)

#Store value for the most recent year data is available, this needs to be changed when data is refreshed annually
FrstYear <- "2010/11"
RcntYear <- "2021/22"
ProjYear <- "2024/25"

#First and last years for Duncan Index graphs
DIFrYr <- substr(FrstYear,1,4)
DIRcYr <- substr(RcntYear,1,4)

LblFrst <- "10/11"
LblRcnt <- "21/22"
LblProj <- "24/25"

SpPolysDF <- read_rds("data/Shapes_decs.rds")
SpPolysIZ <- read_rds("data/IZshapes_decs.rds")
SpPolysLA <- read_rds("data/LAShps.rds")
CPPdta <- read_csv("data/CPPcleandata.csv", show_col_types = FALSE)
CPP_Imp <- read_csv("data/Imp_rate_CPP.csv", show_col_types = FALSE)
IGZdta <- read_csv("data/IGZcleandata.csv", show_col_types = FALSE)
IGZ_latest <- read_csv("data/IGZ_latest.csv", show_col_types = FALSE)
IGZ_change <- read_csv("data/IGZ_change.csv", show_col_types = FALSE)
Metadata <- read_csv("data/Metadata.csv", show_col_types = FALSE)
# VulnComm <- read_csv("data/Formatted Vulnerable Communities.csv", show_col_types = FALSE)
# 
# VulnComm$Most_Deprived_Comm[VulnComm$Most_Deprived_Comm == 6] <- ""
# VulnComm[VulnComm$AreaLabel == "CPP Average", c(4,7,10,13,16,19,22,25,28)] <- ""
# VulnComm <- as.data.frame(VulnComm)

#NEW VIZ DATA FORMAT (calculations in Other Code/7. Vulnerable community calcs.R)
vulnerable_communities_data <- read_csv("data/vulnerable_communities_outcomes_and_change.csv", show_col_types = FALSE)

#rename Edinburgh
SpPolysIZ[SpPolysIZ$council == "Edinburgh","council"] <- "Edinburgh, City of" 
SpPolysDF[SpPolysDF$council == "Edinburgh","council"] <- "Edinburgh, City of" 

#extract data and rename indicator columns (col names will be used directly in leaflet pop-ups in UI)
CPPMapDta <- SpPolysDF %>%
  dplyr::rename("Children in Poverty (%)" = "X..of.children.in.poverty", 
         "Average Highest Attainment" = "Average.highest.attainment",
         "Out of Work Benefits (%)" = "X..of.population..aged.16.64..in.receipt.of.out.of.work.benefits", 
         "Crimes per 10,000" = "Number.of.crimes.per.10.000.of.the.population", 
         "Emergency Admissions (65+) per 100,000" = "Emergency.admissions..65...per.100.000.population")
##convert to numeric
CPPMapDta[[15]] <- as.numeric(CPPMapDta[[15]])
CPPMapDta[[14]] <- as.numeric(CPPMapDta[[14]])


##read in Fife data for MyCommunity
IGZ_latest_Fife <- read_csv("data/IGZ_latest_Fife.csv", show_col_types = FALSE)
IGZ_change_Fife <- read_csv("data/IGZ_change_Fife.csv", show_col_types = FALSE)

#global variables (taken from server)------------
#list of indicators

indicators <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
                  "Attainment", "Positive Destinations", "Employment Rate",
                  "Median Earnings", "Out of Work Benefits", "Business Survival",
                  "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
                  "Emergency Admissions", "Unplanned Hospital Attendances",
                  "Early Mortality", "Fragility", "Well-being", "Fuel Poverty"
)

#Create list of CPP names for use in UI
CPPNames <- unique(CPPdta[CPPdta$CPP != "Scotland", "CPP"])

##Read in Duncan Index Scores and calculate whether improving
DIdta <- read_csv("data/DuncanIndex.csv", show_col_types = FALSE)
DIdta <- DIdta[,-5]
DIdta <- gather(DIdta, "ind", "value",3:9) 
DIdta <- na.omit(DIdta)
DIdta <- setDT(DIdta)[, ImprovementRate :=
                                  (abs(last(value))/abs(first(value)))-1,
                                by = list(la, ind)
                                ]
InqDta <-readRDS("data/DecileData.rds")

#functions ------------

# Map colour functions
# These render polygons red-green (or blue-yellow if the colour blindness button is checked).
# One of seven colours is ascribed depending on a geography's ranking score (1-7) for the given indicator
clrs      <- brewer.pal(7, "RdYlGn")
clrsCB    <- rev(brewer.pal(7, "YlGnBu"))
LowGoodColourBins <- colorBin(rev(clrs), 1:7)
LowGoodColourBinsCB <- colorBin(rev(clrsCB), 1:7)
HighGoodColourBins <- colorBin(clrs, 1:7)
HighGoodColourBinsCB <- colorBin(clrsCB, 1:7)


#renders a plot output with associated metadata pop up for a given indicator. 
# This function is used for the CPP Overt Time, Compare All CPPs, and Compare Similar CPPs tabs.
plotWithMetadataPopup <- function (metadata, plotName, indicatorTitle, plc = "top", plotHeight = "25vh"){
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


#determines the colour of the traffic light marker on each plot in tab "P1" 
trafficLightMarkerColour <- function (data, selected_cpp, comparator_cpp) {
  selected_cpp_data <- filter(data, CPP == selected_cpp)
  comparator_cpp_data <- filter(data, CPP == comparator_cpp)
  
  highIsPositive <- unique(data$`High is Positive`)
  
  if_else(last(selected_cpp_data$value) > last(comparator_cpp_data$value), 
          if_else(last(selected_cpp_data$Improvement_Rate) > last(comparator_cpp_data$Improvement_Rate),
                  if_else(highIsPositive == "Yes",
                          "green",
                          "red"),
                  "yellow"),
          if_else(last(selected_cpp_data$value) < last(comparator_cpp_data$value),
                  if_else(last(selected_cpp_data$Improvement_Rate) < last(comparator_cpp_data$Improvement_Rate),
                          if_else(highIsPositive == "Yes",
                                  "red",
                                  "green"),
                          "yellow"),
                  "black")
  )
}

#adds on-click pop-ups to the data zone maps in the "Map2" tab
showDZpopup <- function(DZdata, group, lat, lng, map_ind, plotId) {
  selectedDZ <- st_drop_geometry(DZdata[DZdata$DataZone == group,]) 
  colIndex <- grep(map_ind, colnames(selectedDZ))
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

#clickable pop-ups for IZ in "Map1"
showIZPopup <- function(group, lat, lng){
  selectedIZ <- SpPolysIZ[SpPolysIZ$InterZone == group,]
  content <- as.character(tagList(
    tags$h4(as.character(unique(selectedIZ$`IGZ.name`)))))
  leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
}

addColourSchemeColumn <- function (dataset, colName, input1, input2 = NULL) {
  colName <- enquo(colName)
  if(is.null(input2)) 
  {
    dta <- dataset %>% 
      mutate(colourscheme = ifelse(!!colName == input1, "A", "C"))
  }
  else
  {
    dta <- dataset %>% 
      mutate(colourscheme = ifelse(!!colName == input1, 
                                    "A",
                                    ifelse(!!colName == input2, 
                                           "B", 
                                           "C")))
  }
  return(dta)
}
