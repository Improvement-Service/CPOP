sidebar <- dashboardSidebar(
  selectizeInput("LA1", "",
                 choices = unique(CPPdta$CPP), options = list(placeholder = "Select a CPP",
                  onInitialize = I('function() { this.setValue(""); }'))),
  sidebarMenu(
  menuItem("Community Maps", tabName = "Map1", icon = icon("map")),
  menuItem("CPP Over Time", tabName = "P1", icon = icon("chart-line")),
  menuItem("Compare All CPPs", tabName = "P2", icon = icon("chart-bar")),
  menuItem("Compare Similar CPPS", tabName = "P3", icon = icon("fort-awesome")),
  menuItem("Data Zone Maps", tabName = "Map2", icon = icon("globe")),
  menuItem("My Communities", tabName = "MyCom", icon = icon("table")),
  menuItem("Community Profile", tabName = "CP", icon = icon("anchor")),
  menuItem("All Communities", tabName = "allCom", icon = icon("heart"))
  )
)

body <- dashboardBody(
  
  tabItems(
    ##Cover Tab
    tabItem(tabName = "Cov",
        tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "plain.css")
            ),
          uiOutput("CovPg")
    ),
###====First tab: all CPPs over time===###    
    tabItem(tabName = "P1",
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
                column(2, plotOutput("plot_1", height = "225px")),
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
        fluidRow(div(class = "row-fluid", 
              conditionalPanel("input.LA1 != ''",       
                     leafletOutput("communityMap", height = 900))))
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
                column(3,
                       uiOutput("CommCP"),
                       tags$style("#Descrip{
                            font-size: 12px;
                            font-style: bold}"),
                       div(textOutput("Descrip")),
                       tags$style("#GrpSize{
                            font-size: 12px;
                            font-style: bold}"),
                       div(textOutput("GrpSize"))
                ),
                column(
                  3,
                  checkboxGroupInput(
                    "IndiCP", 
                    "Select Indicators",
                    unique(IGZdta$Indicator),
                    selected = unique(IGZdta$Indicator)
                  ),
                  radioButtons(
                    "ViewCP", 
                    "Select Display", 
                    c("All", "Top/bottom 10", "Top/bottom 5"),
                    inline = TRUE
                  )
               ),
               column(3, plotOutput("CPplot_1", height = "225px")),
               column(3, plotOutput("CPplot_2", height = "225px"))
              ),
              fluidRow(
                column(1,tags$img(src="Arrow3.png")),
                column(4, DT::dataTableOutput("CommunityProfileTbl")),
                column(1, tags$img(src="Arrow4.png")),
                column(
                  3,
                  plotOutput("CPplot_3", height = "225px"),
                  plotOutput("CPplot_5", height = "225px"),
                  plotOutput("CPplot_7", height = "225px")
                ),
                column(
                  3,
                  plotOutput("CPplot_4", height = "225px"),
                  plotOutput("CPplot_6", height = "225px"),
                  plotOutput("CPplot_8", height = "225px")
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
        )
  )
)

dashboardPage(
  dashboardHeader(title = "CPOP"),
  sidebar,
  body
  )
