sidebar <- dashboardSidebar(
  selectizeInput("LA1", "",
                 choices = unique(CPPdta$CPP), options = list(placeholder = "Select a CPP",
                  onInitialize = I('function() { this.setValue(""); }'))),
  sidebarMenu(
  menuItem("CPP Over Time", tabName = "P1", icon = icon("chart-line")),
  menuItem("Compare All CPPs", tabName = "P2", icon = icon("chart-bar")),
  menuItem("Compare Similar CPPS", tabName = "P3", icon = icon("fort-awesome")),
  menuItem("Community Maps", tabName = "Map1", icon = icon("map")),
  menuItem("Data Zone Maps", tabName = "Map2", icon = icon("globe")),
  menuItem("My Communities", tabName = "MyCom", icon = icon("table")),
  menuItem("Community Profile", tabName = "CP", icon = icon("anchor")),
  menuItem("All Communities", tabName = "allCom", icon = icon("heart")),
  menuItem("Testtt", tabName = "Test", icon = icon("heart"))
  )
)

body <- dashboardBody(
  
  includeCSS("www/background.css"),
  
  tabItems(
###====First tab: all CPPs over time===###    
    tabItem(tabName = "P1",
            conditionalPanel(
              condition = "input.LA1 == ``",
              h1("Please select a CPP using the drop down on the left")
            ),
            conditionalPanel(condition = "input.LA1 != ``",
            fluidRow(
              column(
                6,
                selectInput(
                  "CompLA1", 
                  "Select Comparator", 
                  unique(CPPdta$CPP), 
                  selected = "Scotland"
                )
              ),
              column(6, tags$img(src = "Lgnd1.PNG"))
            ),
              fluidRow(
                column(2, plotOutput("plot_1", height = "225px"),
                       bsPopover(id = "plot_1",
                                  title = "methodology", 
                                  content = "Practice example", 
                                  placement = "top", 
                                  trigger = "click")),
                column(2, plotOutput("plot_2", height = "225px")),
                column(2, plotOutput("plot_3", height = "225px")),
                column(2, plotOutput("plot_4", height = "225px")),
                column(2, plotOutput("plot_5", height = "225px")),
                column(2, plotOutput("plot_6", height = "225px"))
              ),
              fluidRow(
                column(2, plotOutput("plot_7", height = "225px")),
                column(2, plotOutput("plot_8", height = "225px")),
                column(2, plotOutput("plot_9", height = "225px")),
                column(2, plotOutput("plot_10",height = "225px")),
                column(2, plotOutput("plot_11",height = "225px")),
                column(2, plotOutput("plot_12",height = "225px"))
              ),
              fluidRow(
                column(2, plotOutput("plot_13",height = "225px")),
                column(2, plotOutput("plot_14",height = "225px")),
                column(2, plotOutput("plot_15",height = "225px")),
                column(2, plotOutput("plot_16",height = "225px")),
                column(2, plotOutput("plot_17",height = "225px")),
                column(2, plotOutput("plot_18",height = "225px"))
              )
            )
    ),
###====Tab2: Show all Councils for all indicators===###
  tabItem(tabName = "P2",
          fluidPage(
            fluidRow(
            plotOutput("CompCPP")
          )
    )),
###===Tab3: Show only similar councils===###
  tabItem(tabName = "P3",
        fluidPage(
          plotOutput("SimCPP")
        )
    ),
###====Tab4: Show Community maps===###
  tabItem(tabName = "Map1",
        fluidRow(div(class = "row-fluid", leafletOutput("communityMap", height = 900)))
  ),
###===Tab5: Show Data Zone Maps ===###
  tabItem(tabName = "Map2",
          fluidPage(
            absolutePanel(fixed = FALSE,
                          draggable = FALSE, top = "28px", left = 0, right = 0, bottom = 0,
                          width = "100%", height = "0px", style = "opacity:1",
                          wellPanel(
                                div(class = "span4",
                                    uiOutput("IZUI"))
                          )),
            conditionalPanel("input.CPP != 'Select a CPP'", div(class = "row-fluid",
                           fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                        h4("Percentage of Children in Poverty"), 
                        h4("S4 Average Tariff Score"), 
                        h4("% School Leavers Entering Positive Destinations"))
                     ),
                       fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                            leafletOutput("newplot"), 
                            leafletOutput("newplot2"), 
                            leafletOutput("newplot3")))
            ), 
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
          fluidPage(
            tags$head(
              tags$style(HTML("
                              
                              .multicol {
                              
                              -webkit-column-count: 3; /* Chrome, Safari, Opera */
                              
                              -moz-column-count: 3; /* Firefox */
                              
                              column-count: 3;
                              
                              }
                              
                              "))
            ),
            fluidRow(
              column(
                6,
                radioButtons(
                  "View","Select Display",
                  c("All", "Top/bottom 10", "Top/bottom 5"),
                  inline = TRUE)
                ),
              column(
                5,
                tags$div(
                  class = "multicol",
                  checkboxGroupInput(
                    "IndiMyCom",
                    "Select Indicators", 
                    unique(IGZdta$Indicator),
                    selected = unique(IGZdta$Indicator)
                  )
                )
              ),
              column(
                1,
                actionButton("IndiAll","Select All"),
                actionButton("IndiClear", "Clear All")
              )
          ),
          fluidPage(
            fluidRow(
              column(1,tags$img(src = "Arrow1.png")),
              column(10,DT::dataTableOutput("MyCommunitiesTbl")),
              column(1,tags$img(src = "Arrow2.png"))
            )
          )
        )
      ),

##====Tab7: Community profile==========##
    tabItem(tabName = "CP",
            fluidPage(
              fluidRow(
                column(
                  6,
                  fluidRow(
                    column(
                      6,
                      box(
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
                    ,
                    column(
                      6,
                      checkboxGroupInput(
                        "IndiCP", 
                        "Select Indicators",
                        unique(IGZdta$Indicator),
                        selected = unique(IGZdta$Indicator)
                      )
                    )
                  ))),
                  fluidRow(
                    column(
                      6,
                    
                  box(
                    column(2,tags$img(src="Arrow3.png")),
                    column(8, DT::dataTableOutput("CommunityProfileTbl")),
                    column(2, tags$img(src="Arrow4.png")) 
                  )
                ))),
                column(
                  6,
                  box(
                    column(
                      6, 
                      plotOutput("CPplot_1"),
                      plotOutput("CPplot_3"),
                      plotOutput("CPplot_5"),
                      plotOutput("CPplot_7")
                      ),
                    column(
                      6,
                      plotOutput("CPplot_2"),
                      plotOutput("CPplot_4"),
                      plotOutput("CPplot_6"),
                      plotOutput("CPplot_8")
                    )
                  )
                )
              ),
             fluidRow(
                column(6),
                column(2, uiOutput("LineChoicesCP")),
                column(1, tags$img(src = "ComPrflLgnd.PNG")),
                column(1),
                column(
                  1,
                  radioButtons(
                    "ProjectionsCP", 
                    "Show projections?", 
                    c("Yes","No"), 
                    selected = "Yes", 
                    inline = TRUE
                  )
                ),
                column(
                  1,
                  tags$img(src = "DashedLine.PNG")
                )
              )
            )   
    ),            
##===tab8: All Communities===##
  tabItem(tabName = "allCom",
          fluidPage(
            fluidRow(
              column(
                6,
                selectInput(
                  "IndiAllC", 
                  "Select Indicator", 
                  unique(IGZdta$Indicator)
                )
              )
            ),
            hr(),
            plotOutput("AllCPlots")
          )
        ),
tabItem(tabName = "Test",
        fluidPage(
         fluidRow(
            box(
              column(3,
                     selectInput(
                       "test", 
                       "Select Indicator", 
                       unique(IGZdta$Indicator)
                     )),
                     column(3, 
                            selectInput(
                              "test", 
                              "Select Indicator", 
                              unique(IGZdta$Indicator)))
            )
          )
        )
  
)
  )
)

dashboardPage(
  dashboardHeader(title = "CPOP"),
  sidebar,
  body
  )
