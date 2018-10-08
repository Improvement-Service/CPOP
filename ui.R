sidebar <- dashboardSidebar(
  selectizeInput("LA1", "",
                 choices =unique(CPPMapDta[CPPMapDta$council != "Scotland", "council"]), options = list(placeholder = "Select a CPP",
                  onInitialize = I('function() { this.setValue(""); }'))),
  checkboxInput("CBCols", "Colourblind Colour Scheme", value = FALSE),
  sidebarMenu(id = "sidebarmenu",
  menuItem("Community Map", tabName = "Map1", icon = icon("map")),
  menuItem("CPP Over Time", tabName = "P1", icon = icon("line-chart")),
  menuItem("Compare All CPPs", tabName = "P2", icon = icon("bar-chart")),
  conditionalPanel(condition = "input.sidebarmenu == `P2`",checkboxInput("ScotCheckbox", "Show Scotland Value", value = TRUE)),
  menuItem("Compare Similar CPPS", tabName = "P3", icon = icon("area-chart")),
  conditionalPanel(condition = "input.sidebarmenu == `P3`",checkboxInput("ScotCheckbox2", "Show Scotland Value", value = TRUE)),
  menuItem("Data Zone Maps", tabName = "Map2", icon = icon("globe")),
  menuItem("My Communities", tabName = "MyCom", icon = icon("table")),
  menuItem("Community Profile", tabName = "CP", icon = icon("arrow-down")),
  menuItem("All Communities", tabName = "allCom", icon = icon("picture-o")),
  menuItem("Inequality", tabName = "InQ", icon = icon("arrows-v")),
  menuItem("Help Video", tabName = "hVid", icon = icon("video-camera"))
  )
)

body <- dashboardBody(
  tags$head(tags$style(
    ".leaflet{height:36vh !important; border-style:solid; border-width:1px; margin-top:6px}",
    "#communityMap{height:93vh !important;border-style:solid;border-width:1px; margin-left:3px}",
    "#scotMap{height:93vh !important;border-style:solid;border-width:1px; margin-left:3px}",
    ".content{padding-top:1px}",
    ".col-sm-1{padding-left:2px; z-index:1}",
    ".col-sm-10{z-index:2}",
    ".content-wrapper, .right-side {
      background-color: #ffffff;
    }",
    "#comProgressBox{width:100%}",
    "#SimCPP{height:90vh !important}",
    "#CompCPP{height:90vh !important}",
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
         .multicol {
             -webkit-column-count: 3; /* Chrome, Safari, Opera */
             -moz-column-count: 3; /* Firefox */
             column-count: 3;}"))),
  
  tabItems(
###====First tab: all CPPs over time===###    
    tabItem(tabName = "P1",
            fluidPage(fluidRow(
              column(
                6,
                div(style = "margin-top:5px;margin-bottom:20px",
                selectInput(
                  "CompLA1", 
                  "Select Comparator", 
                  c("Scotland",unique(CPPMapDta[CPPMapDta$council != "Scotland", "council"])), 
                  selected = "Scotland"
                ))
              ),
              column(6, tags$img(style = "max-width:100%",src = "Lgnd1.PNG"))
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
            fluidRow(
              div(style = "margin-top:5px",
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
                               leafletOutput("scotMap")%>% withSpinner(type = 6)
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
                2,
                radioButtons(
                  "View","Select Display",
                  c("All", "Top/bottom 10", "Top/bottom 5"),
                  inline = FALSE)),
                column(2,valueBoxOutput("comProgressBox")
                ),
              column(
               7,
                tags$div(
                  class = "multicol",
                  checkboxGroupInput(
                    "IndiMyCom",
                    "Select Indicators", 
                    unique(IGZdta$Indicator),
                    selected = unique(IGZdta$Indicator)
                  )
                )),
               column(1,div(style = "margin-bottom:1px",
                actionButton("IndiAll","Select All")),
                actionButton("IndiClear", "Clear All")
              )
              
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
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12, 
                      column(
                        width = 6,
                        uiOutput("CommCP"),
                        tags$style("#Descrip{
                                   font-size: 13px;
                                   font-style: bold}"),
                        div(textOutput("Descrip")),
                        tags$style("#GrpSize{
                                   font-size: 13px;
                                   font-style: bold}"),
                        div(textOutput("GrpSize")),
                        radioButtons(
                          "ViewCP", 
                          "Select Display", 
                          c("All", "Top/bottom 10", "Top/bottom 5"),
                          inline = TRUE
                        )
                      ),
                      column(
                        width = 6,
                        checkboxGroupInput(
                          "IndiCP", 
                          "Select Indicators",
                          unique(IGZdta$Indicator),
                          selected = unique(IGZdta$Indicator)
                        )
                      )
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      uiOutput("arr2"),
                      column(width = 8, style = "z-index:2",DT::dataTableOutput("CommunityProfileTbl")),
                      column(width = 2, style = "padding-left:2px;z-index:1;", tags$img(style = "max-width:130%",src="Arrow4.PNG"))
                    )
                  )
                ),
                box(
                  width = 6, 
                  column(
                    width = 6,
                    plotOutput("CPplot_1", height = "175px"),
                    plotOutput("CPplot_3", height = "175px"),
                    plotOutput("CPplot_5", height = "175px"),
                    plotOutput("CPplot_7", height = "175px")
                  ),
                  column(
                    width = 6,
                    plotOutput("CPplot_2", height = "175px"),
                    plotOutput("CPplot_4", height = "175px"),
                    plotOutput("CPplot_6", height = "175px"),
                    plotOutput("CPplot_8", height = "175px")
                  ),
                  fluidRow(
                    column(5,uiOutput("LineChoicesCP")),
                    column(7,tags$img(style = "max-width:100%;",src = "ComPrflLgnd.PNG"))
                  ),
                  fluidRow(
                    column(
                      5,
                      radioButtons(
                        "ProjectionsCP", 
                        "Show projections?", 
                        c("Yes","No"), 
                        selected = "Yes", 
                        inline = TRUE
                      )
                    ),
                    column(7,img(style = "max-width:100%;", src = "DashedLine.PNG"))
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
              column(3, tags$img(style = "max-width:100%",src = "allComLgnd.PNG"))
            ),
            hr(),
            plotOutput("AllCPlots") %>% withSpinner(type = 6)
          )
        ),
##====Video Tab===##
  tabItem(tabName = "hVid",
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/3FOzD4Sfgag" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>')
    ),
###=====Inequality tab====##
  tabItem(tabName = "InQ",
          fluidPage(
            fluidRow(
              column(4,
                     selectInput("InqComp", "Select Comparator",
     c("Scotland",unique(CPPMapDta[CPPMapDta$council != "Scotland", "council"])))),
            column(4,
                   selectInput("InqInd", "Select Indicator",c("Child Poverty", "Indi2", "indi3")))
            ),
     tableOutput("inqTbl"),
     plotOutput("InqGrp")
          ))
  )
)

dashboardPage(
  dashboardHeader(title = "CPOP",
                  dropdownMenu(type = "notifications",
                  icon = icon("question-circle"), badgeStatus = NULL,
                  headerText = "Help!!",
                  notificationItem(text = "click here for help", icon = icon("child"),
                                   href = "http://www.improvementservice.org.uk/community-planning-outcomes-profile.html"))),
  sidebar,
  body
  )
