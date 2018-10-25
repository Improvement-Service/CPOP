sidebar <- dashboardSidebar(
  selectizeInput("LA1", "",
                 choices =CPPNames, options = list(placeholder = "Select a CPP",
                  onInitialize = I('function() { this.setValue(""); }'))),
  
  sidebarMenu(id = "tabs",
  menuItem("Community Map", tabName = "Map1", icon = icon("map")),
  menuItem("CPP Over Time", tabName = "P1", icon = icon("line-chart")),
  menuItem("Compare All CPPs", tabName = "P2", icon = icon("bar-chart")),
  menuItem("Compare Similar CPPs", tabName = "P3", icon = icon("area-chart")),
  menuItem("Inequality Over Time", tabName = "InQ", icon = icon("arrows-v")),
  menuItem("My Communities", tabName = "MyCom", icon = icon("table")),
  menuItem("Community Profile", tabName = "CP", icon = icon("arrow-down")),
  conditionalPanel(condition = "input.tabs == `CP`", uiOutput("CommCP")),
  menuItem("All Communities", tabName = "allCom", icon = icon("picture-o")),
  menuItem("Data Zone Comparison", tabName = "Map2", icon = icon("globe")),
  menuItem("Data Download", tabName = "DtaDL", icon = icon("download")),
  awesomeCheckbox("CBCols", "Colour Blind Colour Scheme", value = FALSE),
  tags$footer(a("Contact us", href = "mailto:nicholas.cassidy@improvementservice.org.uk"), style = "position:fixed; bottom:0; margin-left:2px")
  )
)

body <- dashboardBody(
  tags$head(tags$style(
    ".leaflet{height:36vh !important; border-style:solid; border-width:1px; margin-top:6px}",
    "#communityMap{height:91vh !important;border-style:solid;border-width:1px; margin-left:3px}",
    "#scotMap{height:91vh !important;border-style:solid;border-width:1px; margin-left:3px}",
    ".content{padding-top:1px}",
    ".col-sm-1{padding-left:2px; z-index:1}",
    ".col-sm-10{z-index:2}",
    ".content-wrapper, .right-side {
      background-color: #ffffff;
    }",
    "#comProgressBox{width:80%; padding-right:0px; padding-left:0px}",
    "#SimCPP{height:90vh !important}",
    "#CompCPP{height:75vh !important; margin-top:15px}",
    ".main-header .logo {text-align:left; padding-left:0px}",
    #".shiny-plot-output{26vh !important}",
    "#plot_1{height:25vh ! important}",
    "#plot_2{height:25vh ! important}",
    "#plot_3{height:25vh ! important}",
    "#plot_4{height:25vh ! important}",
    "#plot_5{height:25vh ! important}",
    "#plot_6{height:25vh ! important}",
    "#plot_7{height:25vh ! important}",
    "#plot_8{height:25vh ! important}",
    "#plot_9{height:25vh ! important}",
    "#plot_10{height:25vh ! important}",
    "#plot_11{height:25vh ! important}",
    "#plot_12{height:25vh ! important}",
    "#plot_13{height:25vh ! important}",
    "#plot_14{height:25vh ! important}",
    "#plot_15{height:25vh ! important}",
    "#plot_16{height:25vh ! important}",
    "#plot_17{height:25vh ! important}",
    "#plot_18{height:25vh ! important}",
    ".btn-group.bootstrap-select.form-control {background: border-box}",
    HTML(" h5{height: 18px;
         margin-top:2px;
         margin-bottom:0px;
         text-align:centre;
         font-weight: bold;}
         h4 {
         font-size:12px;
         height: 18px;
         margin-top:2px;
         margin-bottom:0px;
         text-align:centre;
         font-weight: bold;}
        h3{font-style:italic;
          font-family: sans-serif;
        font-height:3vh}
      h2{font-family: sans-serif;
          font-weight:bold;
          font-size:5vh; 
          margin-top:6vh}
        strong{float:right;}
          .small-box {margin-bottom:1px}
          .small-box >.inner {padding:5px}
         "))),
  
  tabItems(
###====First tab: all CPPs over time===###    
    tabItem(tabName = "P1",
            fluidPage(fluidRow(
              column(
                4,
                div(style = "margin-top:5px;margin-bottom:20px",
                selectInput(
                  "CompLA1", 
                  "Select Comparator", 
                  c("Scotland",CPPNames), 
                  selected = "Scotland"
                ))
              ),
              column(5, style = "margin-top:1px",div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Selection.png"),
                                                     span(textOutput("CPPLgnd"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                     div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Comp.png"),
                         span(textOutput("CompLgnd"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block"))
              )
              ),
              fluidRow(style = "margin-bottom:10px; margin-right:1px",
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_1"),
                       bsPopover(id = "plot_1",
                                  title = "Healthy Birthweight", 
                                  content = paste(
                                    "<b>Definition</b></p><p>",
                                    DefHBW,
                                    "</p><p>",
                                    "<b>Raw Time Period</b></p><p>",
                                    TimeHBW,
                                    "</p><p>",
                                    "<b>Source</b></p><p>",
                                    SourceHBW
                                  ),
                                  placement = "bottom",
                                  trigger = "click",
                                  options = list(container = "body")
                                 )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_2"),
                       bsPopover(id = "plot_2",
                                 title = "Primary 1 Body Mass Index", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefBMI,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeBMI,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceBMI
                                 ),
                                 placement = "bottom",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_3"),
                       bsPopover(id = "plot_3",
                                 title = "Child Poverty", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefCPov,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeCPov,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceCPov
                                 ),
                                 placement = "bottom",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_4"),
                       bsPopover(id = "plot_4",
                                 title = "S4 Tariff", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefS4T,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeS4T,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceS4T
                                 ),
                                 placement = "bottom",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_5"),
                       bsPopover(id = "plot_5",
                                 title = "Positive Destinations", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefPosDes,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimePosDes,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourcePosDes
                                 ),
                                 placement = "bottom",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_6"),
                       bsPopover(id = "plot_6",
                                 title = "Employment Rate", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefEmpRt,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeEmpRt,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceEmpRt
                                 ),
                                 placement = "bottom",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       )
              ),
              fluidRow(style = "margin-bottom:10px;margin-right:1px",
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_7"),
                       bsPopover(id = "plot_7",
                                 title = "Median Earnings", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefMedEarn,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeMedEarn,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceMedEarn
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_8"),
                       bsPopover(id = "plot_8",
                                 title = "Out of Work Benefits", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefOWB,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeOWB,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceOWB
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_9"),
                       bsPopover(id = "plot_9",
                                 title = "Business Survival", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefBusSurv,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeBusSurv,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceBusSurv
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_10"),
                       bsPopover(id = "plot_10",
                                 title = "Crime Rate", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefCrime,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeCrime,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceCrime
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_11"),
                       bsPopover(id = "plot_11",
                                 title = "Dwelling Fires", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefFire,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeFire,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceFire
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_12"),
                       bsPopover(id = "plot_12",
                                 title = "Carbon Emissions", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefEmiss,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeEmiss,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceEmiss
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       )
              ),
              fluidRow(style = "margin-bottom:0px;margin-right:1px",
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_13"),
                       bsPopover(id = "plot_13",
                                 title = "Emergency Admissions", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefEmAd,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeEmAd,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceEmAd
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_14"),
                       bsPopover(id = "plot_14",
                                 title = "Unplanned Hospital Attendances", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefHospAtt,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeHospAtt,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceHospAtt
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_15"),
                       bsPopover(id = "plot_15",
                                 title = "Early Mortality", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefMort,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeMort,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceMort
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_16"),
                       bsPopover(id = "plot_16",
                                 title = "Fragility", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefFrag,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeFrag,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceFrag
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_17"),
                       bsPopover(id = "plot_17",
                                 title = "Well-being", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefWellB,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeWellB,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceWellB
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       ),
                column(2, style = "margin-left:0px;margin-right:0px;padding-right:0px", plotOutput("plot_18"),
                       bsPopover(id = "plot_18",
                                 title = "Fuel Poverty", 
                                 content = paste(
                                   "<b>Definition</b></p><p>",
                                   DefFuelPov,
                                   "</p><p>",
                                   "<b>Raw Time Period</b></p><p>",
                                   TimeFuelPov,
                                   "</p><p>",
                                   "<b>Source</b></p><p>",
                                   SourceFuelPov
                                 ),
                                 placement = "top",
                                 trigger = "click",
                                 options = list(container = "body")
                       )
                       )
              )
    )),
###====Tab2: Show all Councils for all indicators===###
  tabItem(tabName = "P2",
          fluidPage(
            fluidRow(style = "padding-top:10px",
              column(4,style = "margin-top:3px",uiOutput("CompSelection")),
              column(5, style = "margin-top:1px",div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Selection.png"),
                       span(textOutput("BarLA"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                     div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Comp.png"),
                         span(textOutput("BarScot"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                     conditionalPanel("input.OtherCPP != ''",
                                      div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - LA.png"),
                                          span(textOutput("BarComp"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")))
              )
            ),
            fluidRow(
              div(style = "margin-top:10px",
            plotOutput("CompCPP")%>% withSpinner(type = 6)
          ))
          
    )),
###===Tab3: Show only similar councils===###
  tabItem(tabName = "P3",
        fluidPage(
          div(style = "margin-top:5px",
          plotOutput("SimCPP")%>% withSpinner(type = 6)
        ))
    ),
###====Tab4: Show Community maps===###
  tabItem(tabName = "Map1",
        fluidRow(
              conditionalPanel("input.LA1 == ''", 
                              column(6,
                                  h2("Welcome to the Community Planning Outcomes Profile (CPOP)"),
                                   h3("The CPOP tool aims to help you assess if the lives of people in your community are improving by providing a set of core measures on important life outcomes including early years, older people, safer/stronger communities, health and wellbeing, and engagement with local communities and a consistent basis for measuring outcomes and inequalities of outcome in your area."),
                               h3("To get started use the map on the right to select a CPP and the communities that make up that CPP, and don’t forget to look at ‘help with this page’ in the top right hand corner of every page, as that gives a useful introduction to how to use each page. To explore others parts of the CPOP use the list on left to help you navigate the tool.")),
                               leafletOutput("scotMap",width = "50%")
              ),
              conditionalPanel("input.LA1 != ''",       
                     leafletOutput("communityMap") %>% withSpinner(type = 6)))
  ),
###===Tab5: Show Data Zone Maps ===###
  tabItem(tabName = "Map2",
          fluidPage(
                                    div(style = "margin-left:40px", uiOutput("IZUI")),
            conditionalPanel("input.CPP != ' '", 
                           fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                        h4("Percentage of Children in Poverty"), 
                        h4("S4 Average Tariff Score"), 
                        h4("% School Leavers Entering Positive Destinations"))
                     ),
                       fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                            leafletOutput("newplot"), 
                            leafletOutput("newplot2"), 
                            leafletOutput("newplot3"))), 
            hr(style = "margin-bottom:0.3vh; margin-top:0.5vh")),
            conditionalPanel("input.CPP != 'Select a CPP'",
               fluidRow(
              splitLayout(cellWidths = c("33%", "33%", "33%"),
                      h4("% Aged 16-64 Receiving Out of Work Benefits"), 
                      h4("Number of SIMD Crimes per 10,000 People"), 
                      h4("Emergency Admissions (65+) per 100,000 People"))
                ),  
                fluidRow(
                  splitLayout(cellWidths = c("33%", "33%", "33%"),
                     leafletOutput("newplot4"), 
                     leafletOutput("newplot5"), 
                     leafletOutput("newplot6")))
            )
            
          ) 
      ),
###=== Tab6: My Communities ===###
  tabItem(tabName = "MyCom",
          fluidPage(style = "padding-right:30px,overflow-y: auto;",
                    tags$head(
                      tags$style(HTML("
                              .multicol {
                              height:75px;
                              
                              -webkit-column-count: 3; /* Chrome, Safari, Opera */
                              
                              -moz-column-count: 3; /* Firefox */
                              
                              column-count: 3;
                              
                              }
                              
                              "))
                    ),
            fluidRow(
              column(
                2, style = "padding-right:0px; padding-left:5px",
                awesomeRadio(
                  "View","Select Display",
                  c("Top/bottom 5","Top/bottom 10","All"),
                  inline = FALSE)),
                column(3, style= "padding-left:0px",valueBoxOutput("comProgressBox"),
                       checkbox = TRUE
                ),
              column(
               7,
                tags$div(
                  class = "multicol",
                  awesomeCheckboxGroup(
                    "IndiMyCom",
                    "Select indicators",
                    unique(IGZdta$Indicator),
                    selected = unique(IGZdta$Indicator)
                  )

                ))#,
#               column(1,div(style = "margin-bottom:1px",
  #              actionButton("IndiAll","Select All")),
  #              actionButton("IndiClear", "Clear All")
   #           )
          ),
         
            fluidRow(
              uiOutput("arr1"),
              column(10,div(style = "margin-left:9px",DT::dataTableOutput("MyCommunitiesTbl"))),
              column(1,div(tags$img(style = "max-width:150%; width:150%",src = "Arrow2.PNG")))
            )
           
        )
      ),

##====Tab7: Community profile==========##
    tabItem(tabName = "CP",
            fluidPage(
              tags$head(
                tags$style(HTML("
                              .multicol {
                              height:75px;
                              
                              -webkit-column-count: 3; /* Chrome, Safari, Opera */
                              
                              -moz-column-count: 3; /* Firefox */
                              
                              column-count: 3;
                              
                              }
                              
                              "))
              ),
              fluidRow(
                column(
                  4,
                  awesomeRadio(
                    "ViewCP", 
                    "Select Display", 
                    c("All", "Top/bottom 10", "Top/bottom 5"),
                    inline = TRUE,
                    checkbox = TRUE
                  )
                ),
                column(
                  8,
                  tags$div(
                    class = "multicol", 
                    awesomeCheckboxGroup(
                    "IndiCP", 
                    "Select Indicators",
                    unique(IGZdta$Indicator),
                    selected = unique(IGZdta$Indicator)
                  )
                  )
                ),
                fluidRow(
                  column(6,
                  style = "padding-left:0px",box(
                    width = 12,
                    uiOutput("arr2"),
                      column(width = 8, style = "z-index:2",DT::dataTableOutput("CommunityProfileTbl")),
                      column(width = 2, style = "padding-left:2px;z-index:1;", tags$img(style = "max-width:130%",src="Arrow4.PNG"))
                    )
                ),
                 column(6,
                 style = "padding-left:0px",box(
                  width = 12, 
                  plotOutput("CPplots", height = "700px"),
                  fluidRow(
                    column(
                      7,
                      uiOutput("LineChoicesCP")
                    ),
                    column(
                      5,
                      tags$img(style = "max-width:100%;",src = "ComPrflLgnd.PNG")
                      )
                  ),
                  fluidRow(
                    column(
                      5,
                     shinyjs::useShinyjs(),
                      uiOutput("AddComm")
                      #conditionalPanel(condition = "input.ChoicesCP.indexOf(`Similar Community`) != -1",uiOutput("AddComm"))
                    ),
                    column(
                      7,
                      tags$style("#Descrip{
                                   font-size: 13px;
                                 font-style: bold}"),
                      div(textOutput("Descrip")),
                      tags$style("#GrpSize{
                                 font-size: 13px;
                                 font-style: bold}"),
                      div(textOutput("GrpSize"))
                    )
                  ),
                  fluidRow(
                    column(
                      7,
                      awesomeRadio(
                        "ProjectionsCP", 
                        "Show projections?", 
                        c("Yes","No"), 
                        selected = "Yes", 
                        inline = TRUE,
                        checkbox = TRUE
                      )
                    ),
                    column(5,img(style = "max-width:100%;", src = "DashedLine.PNG"))
                  )
                )
            )
              )  
              )
            )
    ),            
##===tab8: All Communities===##
  tabItem(tabName = "allCom",
          fluidPage(
            fluidRow(
              column(
                4,
                selectInput(
                  "IndiAllC", 
                  "Select Indicator", 
                  unique(IGZdta$Indicator)
                )
              ),
              column(5, style = "margin-top:1px",div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Selection.png"),
                                                     span(textOutput("CommLgnd"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                     div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - LA.png"),
                         span(textOutput("CPPLgnd2"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                     div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Comp.png"),
                         span(textOutput("ScotLgnd"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block"))
              )),
            hr(),
            plotOutput("AllCPlots") %>% withSpinner(type = 6)
          )
        ),

###=====Inequality tab====##
  tabItem(tabName = "InQ",
          fluidPage(
            fluidRow(
              column(4,
                     uiOutput("ICompUI")),
            column(3,
                   selectInput("InqYr", "Select Year", unique(IGZdta$Year)[1:11], selected = "2016/17")),
            column(5, style = "margin-top:1px",div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Selection.png"),
                   span(textOutput("CPPLgndInq"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                   div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Comp.png"),
                   span(textOutput("CompLgndInq"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block"))
                   )
            ),
     tableOutput("inqTbl"),
     hr(),
     plotOutput("InqGrp"),
     fluidRow(),
     strong("Methodology Source: University of Sheffield")
          )),

##Download tab====================##
  tabItem(tabName = "DtaDL",
          fluidPage(
            fluidRow(h3("Download the Data"), p("Use these buttons to download all of the data used in this tool. Please note that much of this data is modelled and so may not match exactly with data from other sources."),
                     downloadBttn("DLDta",label = "Download All CPP Data"),
                     downloadBttn("DLIZDta", label = "Download All Community Data", style = "fill", color = "success"),
                     hr()),
            fluidRow(h3("Methodology"), p("You can find details on the methodology used to collect all of our indicators", style = "display:inline"), a("here", href = "https://khub.net/"), hr()),
            fluidRow(h3("Other Profiling Tools and Data Sources"), p("There are a number of other profiling tools available, some of these are listed below.\nYou can also find sources for some of the data used in this tool"),
                                                    tags$ul( 
                                                      #Link to ScotPHO
                                                      tags$li(class= "li-custom", tags$a(href="https://scotland.shinyapps.io/ScotPHO_profiles_tool/", 
                                                                                         "ScotPHO profiles",  class="externallink", target = "_blank")),
                                                      #Link to GCPH
                                                      tags$li(class= "li-custom", tags$a(href="http://www.understandingglasgow.com/",
                                                                                         "Glasgow Centre for Population Health (GCPH)",  class="externallink", target = "_blank")), 
                                                      #Link to Fife
                                                      tags$li(class= "li-custom", tags$a(href="https://knowfife.fife.gov.uk/",
                                                                                         "KnowFife Dataset",  class="externallink", target = "_blank")), 
                                                      #Link to NRS
                                                      tags$li(class= "li-custom", tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/council-area-profiles", 
                                                                                         "National Records of Scotland (NRS) Council Area Profiles",  class="externallink", target = "_blank")), 
                                                      #Link to stats.gov.scot
                                                      tags$li(class= "li-custom", tags$a(href="http://statistics.gov.scot/home", 
                                                                                         "Statistics.gov.scot",  class="externallink")), 
                                                      #Link to Scotland's environment
                                                      tags$li(class= "li-custom", tags$a(href="http://www.environment.gov.scot/", 
                                                                                         "Scotland's Environment Hub",  class="externallink", target = "_blank")),
                                                      #Link to Stats.gov
                                                      tags$li(class= "li-custom", tags$a(href="https://statistics.gov.scot/home", 
                                                                                         "Statistics.gov.scot",  class="externallink", target = "_blank")),
                                                      tags$li(class= "li-custom", tags$a(href="https://www.nomisweb.co.uk/", 
                                                                                         "NOMIS - Official Labour Market Statistics",  class="externallink", target = "_blank")),
                                                      tags$li(class= "li-custom", tags$a(href="https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml", 
                                                                                         "DWP - Stat-Xplore",  class="externallink", target = "_blank"))
                                                      
                                                      
                                                    ))
          )
          ))
    
##Footer========================##
#  tags$footer("Some text",
#              style = "
#              position:absolute;
#              bottom:0;
#              width:100%;
#              height:20px;   /* Height of the footer */
#              color: white;
#              padding: 10px;
#              background-color: black;
#              z-index: 1000;"
#  )
  
)

dashboardPage(
  dashboardHeader(
    title = "CPOP",
    tags$li(
      class = "dropdown", 
      tags$head(
        tags$style(HTML('#HelpButton{background-color:White;
                                     font-size: 20px;
                                     font-weight: 600
                                    }')
        )
      ),
      div(style = "padding-right:8px; padding-top:5px",actionBttn("HelpButton", "Help with this page", icon = icon("question-circle"), style = "jelly")))
    ),
  sidebar,
  body
  )
