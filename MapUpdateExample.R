library(shiny)
library(leaflet)
library(sp)

##Use this link to download shapes: https://drive.google.com/open?id=1vA4hwCslvAIxboXp4WI9Rt9I8bT0kdH3

##read shapefile - change location to wherever you save shapes
LaShapes <- readRDS("data/LAShps.rds")

##This is the list of local authorities
CPPs <- c("Aberdeen City" ,"Aberdeenshire"  ,       "Angus"  ,              
"Argyll and Bute"   ,    "Clackmannanshire"  ,    "Dumfries and Galloway",
"Dundee City"    ,       "East Ayrshire"   ,      "East Dunbartonshire" , 
"East Lothian"    ,      "East Renfrewshire"  ,   "Edinburgh, City of"  , 
"Eilean Siar"    ,       "Falkirk"   ,            "Fife"       ,          
"Glasgow City"     ,     "Highland"    ,          "Inverclyde"       ,    
"Midlothian"        ,    "Moray"         ,        "North Ayrshire"    ,   
"North Lanarkshire"  ,   "Orkney Islands" ,       "Perth and Kinross"  ,  
"Renfrewshire"        ,  "Scotland"        ,      "Scottish Borders"    , 
"Shetland Islands"     , "South Ayrshire"   ,     "South Lanarkshire"    ,
"Stirling"              ,"West Dunbartonshire",   "West Lothian"    )

ui <- pageWithSidebar(
##set up UI page
  headerPanel("Example map"),
  
  sidebarPanel(selectizeInput("LA1", "",
               choices = CPPs, options = list(placeholder = "Select a CPP",
                       onInitialize = I('function() { this.setValue(""); }')))),
  
  mainPanel(leafletOutput("exampleLeaflet"))
  
)

server <- function(input, output, session){
  
##Create the leaflet map that we can click on
  output$exampleLeaflet <- renderLeaflet({
    leaflet(LaShapes) %>%
      addTiles() %>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~NAME,
        fillColor = "grey", 
        color = "black",
        label = LaShapes@data$NAME,
        highlightOptions = highlightOptions(color = "white", weight = 3,bringToFront = TRUE)
      )
  })
  
##This updates the input to whatever we click
  ##Click to select the CPP
  observe({
    event <- input$exampleLeaflet_shape_click
    if(is.null(event)){
      return()} 
    isolate({
      
      updateSelectizeInput(session,"LA1", label = NULL, choices = NULL, selected = event$id)
    })
  })
}

shinyApp(ui, server)
