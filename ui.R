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
  menuItem("All Communities", tabName = "allCom", icon = icon("heart"))
  )
)

body <- dashboardBody(
  
  tabItems(
###====First tab: all CPPs over time===###    
    tabItem(tabName = "P1",
            conditionalPanel(condition = "input.LA1 == ``",
                          h1("Please select a CPP using the drop down on the left")),
            conditionalPanel(condition = "input.LA1 != ``",
            fluidRow(
              column(4,
                     selectInput("CompLA1", "Select Comparator", unique(CPPdta$CPP), selected = "Scotland")
              ),
              column(4,
                     selectInput("Indi1", "Select Indicator", c("All", unique(CPPdta$Indicator)), selected = "All")
              ),
              column(2, tags$hr(style="border-color: red;")
              ),
              column(2
              ),
              column(2, tags$hr(style="border-color: #48CCCD;")
              )
            )),
            hr(),
            conditionalPanel(
              condition = "input.Indi1 == 'All'",
              fluidRow(
                column(3, plotOutput("plot_1"),
                plotOutput("plot_2"),
                plotOutput("plot_3"),
                plotOutput("plot_4"),
                plotOutput("plot_17")),
                column(3, plotOutput("plot_5"),
                plotOutput("plot_6"),
                plotOutput("plot_7"),
                plotOutput("plot_8"),
                plotOutput("plot_13")),
                column(3, plotOutput("plot_9"),
                plotOutput("plot_10"),
                plotOutput("plot_11"),
                plotOutput("plot_12")),
                column(3, plotOutput("plot_14"),
                plotOutput("plot_15"),
                plotOutput("plot_16"),
                plotOutput("plot_18"))
              )
            ),
            conditionalPanel(
              condition = "input.Indi1 != 'All'",
              mainPanel(
                plotOutput("Indi1Plot")
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
              column(6,
                       radioButtons("View","Select Display",c("All", "Top/bottom 10", "Top/bottom 5"),inline = TRUE)),
              column(5,
                     tags$div(class = "multicol",checkboxGroupInput("Indi4","Select Indicators", unique(IGZdta$Indicator),selected = unique(IGZdta$Indicator)))),
              column(1,
                     actionButton("IndiAll","Select All"),
                     actionButton("IndiClear", "Clear All"))
            ),
            fluidPage(
              fluidRow(
                column(1,
                       tags$img(src = "Arrow1.png")),
                column(10,
                       DT::dataTableOutput("MyCommunitiesTbl")
                ),
                column(1,
                       tags$img(src = "Arrow2.png"))
              )
              
            )
            
              )        
          
          ),

##====Tab7: Community profile==========##
    tabItem(tabName = "CP",
            fluidPage(
              fluidRow(
                column(3,
                       uiOutput("Comm5"),
                       h3(textOutput("Descrip")),
                       h3(textOutput("GrpSize"))
                ),
                column(3,
                       checkboxGroupInput("Indi5", "Select Indicators",unique(IGZdta$Indicator),selected = unique(IGZdta$Indicator)),
                       radioButtons("View5", "Select Display", c("All", "Top/bottom 10", "Top/bottom 5"),inline = TRUE)
                ),
                column(3,
                       uiOutput("LineChoices5"),
                       radioButtons("Projections5", "Show projections?", c("Yes","No"), selected = "Yes", inline = TRUE)
                ),
                column(3,
                       plotOutput("5plot_1")
                )
              ),
              fluidRow(
                column(1,
                       tags$img(src="Arrow3.png")),
                column(4,
                       DT::dataTableOutput("CommunityProfileTbl")),
                column(1,
                       tags$img(src="Arrow4.png")),
                column(3, 
                       plotOutput("5plot_2"),
                       plotOutput("5plot_4"),
                       plotOutput("5plot_6")),
                column(3,
                       plotOutput("5plot_3"),
                       plotOutput("5plot_5"),
                       plotOutput("5plot_7"))
              )
              
            )     
            ),

##===tab8: All Communities===##
  tabItem(tabName = "allCom",
          fluidPage(
            fluidRow(column(6,selectInput("Indi-AllC", "Select Indicator", unique(IGZdta$Indicator)))),
            hr(),
            plotOutput("AllCPlots")
          ))

))

dashboardPage(
  dashboardHeader(title = "CPOP"),
  sidebar,
  body
  )
