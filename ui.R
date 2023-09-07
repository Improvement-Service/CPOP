#sidebar-----------------------
sidebar <- dashboardSidebar(
  selectizeInput("LA1", "Select a CPP:",
                 choices =CPPNames, options = list(placeholder = "CPP",
                                                   onInitialize = I('function() { this.setValue(""); }'))),
  sidebarMenu(id = "tabs",
              menuItem("Community Map", tabName = "Map1", icon = icon("map")),
              sidebarMenuOutput("firstHalfMenu"), #placeholder for hidden menu items (shown only when user selects LA)
              uiOutput("communityDropDown"),
              sidebarMenuOutput("secondHalfMenu"),
              awesomeCheckbox("CBCols", "Colour Blind Colour Scheme", value = FALSE),
              tags$footer(a("Contact us", href = "mailto:benchmarking@improvementservice.org.uk"), style = "position:fixed; bottom:0; margin-left:2px")
  )
) #end of sidebar

#body start-----------------------
body <- dashboardBody(
  tags$head(  #the following three lines include cookie control and google analytics in the app
    HTML("<script src='https://cc.cdn.civiccomputing.com/9/cookieControl-9.x.min.js'></script>"),
    includeHTML("google-analytics.html"),
    includeHTML("cookie-control-cpop.html"),
    tags$style(
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
      "#vulnerable_change{width:80vw !important; height:100vh !important}",
      "#vulnerable_outcomes_plot{width:80vw !important}",
      "#vulnerable_change_text{width:70vw !important}",
      "#community_bump_chart{width:80vw !important}",
      
      "#SimCPP{height:90vh !important}",
      "#CompCPP{height:75vh !important; margin-top:15px}",
      ".main-header .logo {text-align:left; padding-left:0px}",
      "#DLDta_bttn{margin-right:10px}",
      "#Map1P1{margin-left:20px}",
      "#P1P1{margin-left:20px}",
      "#MyComP1{margin-left:20px}",
      "#MyComP2{margin-left:20px}",
      "#CPP1{margin-left:20px}",
      "#CPP2{margin-left:20px}",
      ".popover{width:40vw; max-width:450px}",
      ".btn-group.bootstrap-select.form-control {background: border-box}",
      ".skin-blue {padding-right:0px}",
      "#VulnTable {margin-top:10px}",
      "#HeaderVuln {font-style:italic;
          font-family: sans-serif;
        font-size:3vh; margin-top:5px;
    }",
      HTML(" h5{height: 18px;
         margin-top:2px;
         margin-bottom:0px;
         text-align:centre;
         font-weight: bold;}
         h4 {font-size:12px;
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
          margin-top:6vh;
          text-decoration:underline}
        strong{float:right;}
          .small-box {margin-bottom:1px}
          .small-box >.inner {padding:5px}
         "))
  ),
  
  
  
  tabItems(
    # Community Map ("Map1")------------------------
    tabItem(tabName = "Map1",
            fluidRow(
              conditionalPanel("input.LA1 == ''", 
                               column(12,leafletOutput("scotMap",width = "100%") %>% withSpinner(type = 6))
              ),
              conditionalPanel("input.LA1 != ''",       
                               leafletOutput("communityMap") %>% withSpinner(type = 6)))
    ), #end of Community Map ("Map1")
    
    # CPP over time ("P1")------------------------- 
    tabItem(tabName = "P1",
            fluidPage(fluidRow(
              tags$div(style = "position: absolute; top: -100px;",
                       textOutput("clock")
              ),
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
            div(style = "margin-top:10px",
                fluidRow(style = "margin-bottom:0px;margin-right:1px",
                         plotWithMetadataPopup(Metadata, "plot_1", "Healthy Birthweight", "bottom"),
                         plotWithMetadataPopup(Metadata, "plot_2", "Primary 1 Body Mass Index", "bottom"),
                         plotWithMetadataPopup(Metadata, "plot_3", "Child Poverty", "bottom"),
                         plotWithMetadataPopup(Metadata, "plot_4", "Average Highest Attainment", "bottom"),
                         plotWithMetadataPopup(Metadata, "plot_5", "Positive Destinations", "bottom"),
                         plotWithMetadataPopup(Metadata, "plot_6", "Employment Rate", "bottom")
                ),
                fluidRow(style = "margin-bottom:0px;margin-right:1px",
                         plotWithMetadataPopup(Metadata, "plot_7", "Median Earnings"),
                         plotWithMetadataPopup(Metadata, "plot_8", "Out of Work Benefits"),
                         plotWithMetadataPopup(Metadata, "plot_9", "Business Survival"),
                         plotWithMetadataPopup(Metadata, "plot_10", "Crime Rate"),
                         plotWithMetadataPopup(Metadata, "plot_11", "Dwelling Fires"),
                         plotWithMetadataPopup(Metadata, "plot_12", "Carbon Emissions")
                ),
                fluidRow(style = "margin-bottom:0px;margin-right:1px",
                         plotWithMetadataPopup(Metadata, "plot_13", "Emergency Admissions"),
                         plotWithMetadataPopup(Metadata, "plot_14", "Unplanned Hospital Attendances"),
                         plotWithMetadataPopup(Metadata, "plot_15", "Early Mortality"),
                         plotWithMetadataPopup(Metadata, "plot_16", "Fragility"),
                         plotWithMetadataPopup(Metadata, "plot_17", "Well-being"),
                         plotWithMetadataPopup(Metadata, "plot_18", "Fuel Poverty")
                )
            )  
            )), #end of CPP Over Time ("P1")
    
    # Compare All CPPs ("P2")-------------------------------
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
              div(style = "margin-top:10px",
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plot_CPP_1", "Healthy Birthweight", "bottom", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_2", "Primary 1 Body Mass Index", "bottom", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_3", "Child Poverty", "bottom", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_4", "Average Highest Attainment", "bottom", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_5", "Positive Destinations", "bottom", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_6", "Employment Rate", "bottom", plotHeight = "28vh")
                  ),
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plot_CPP_7", "Median Earnings", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_8", "Out of Work Benefits", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_9", "Business Survival", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_10", "Crime Rate", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_11", "Dwelling Fires", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_12", "Carbon Emissions", plotHeight = "28vh")
                  ),
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plot_CPP_13", "Emergency Admissions", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_14", "Unplanned Hospital Attendances", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_15", "Early Mortality", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_16", "Fragility", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_17", "Well-being", plotHeight = "28vh"),
                           plotWithMetadataPopup(Metadata, "plot_CPP_18", "Fuel Poverty", plotHeight = "28vh")
                  )
              )
              
            )), #end of Compare All CPPs ("P2")
    
    # Compare Similar CPPs ("P3")---------------------------------
    tabItem(tabName = "P3",
            fluidPage(
              div(style = "margin-top:5px",
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plotSimCPP_1", "Healthy Birthweight", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_2", "Primary 1 Body Mass Index", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_3", "Child Poverty", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_4", "Average Highest Attainment", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_5", "Positive Destinations", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_6", "Employment Rate", "bottom", plotHeight = "30vh")
                  ),
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plotSimCPP_7", "Median Earnings", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_8", "Out of Work Benefits", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_9", "Business Survival", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_10", "Crime Rate", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_11", "Dwelling Fires", "bottom", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_12", "Carbon Emissions", "bottom", plotHeight = "30vh")
                  ),
                  fluidRow(style = "margin-bottom:0px;margin-right:1px",
                           plotWithMetadataPopup(Metadata, "plotSimCPP_13", "Emergency Admissions", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_14", "Unplanned Hospital Attendances", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_15", "Early Mortality", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_16", "Fragility", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_17", "Well-being", plotHeight = "30vh"),
                           plotWithMetadataPopup(Metadata, "plotSimCPP_18", "Fuel Poverty", plotHeight = "30vh")
                  )
              ))
    ), #end of Compare All CPPs ("P3")
    
    # Inequality Over Time ("InQ") ----------------
    tabItem(tabName = "InQ",
            fluidPage(
              fluidRow(
                column(4,
                       uiOutput("ICompUI"))
              ),
              div(h3("Inequality Between Most and Least Deprived Communities", style = "margin-top:1px"), style = "margin-top:1px"),
              tableOutput("inqTbl"),
              hr(style = "height:2px;background-color:black;color:black"),
              fluidRow(column(5,h3("Inequality Across All Communities", style = "margin-top:2px")),
                       column(5, style = "margin-top:1px",div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Selection.png"),
                                                              span(textOutput("CPPLgndInq"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block")),
                              div(style = "display:block",tags$img(style = "margin-right:2px",src = "Legend - Comp.png"),
                                  span(textOutput("CompLgndInq"), style = "font-size:1.4vw;; font-weight:bold; display:inline-block"))
                       )),
              plotOutput("InqGrp"),
              fluidRow(),
              div(em("These graphs will help you understand inequality in outcomes across the whole of the CPP, with 0 indicating perfect equality, values between 0 and 1 indicating that income deprived people experience poorer outcomes, and values between -1 and 0 indicating that non-income deprived people experience poorer outcomes. Please note that this is experimental analysis which makes use of modelled data alongside raw data."),
                  strong("Methodology Source: University of Sheffield"))
            )), #end of Inequality Over Time ("InQ")
    # Vulnerable Communities ("Vuln")-------------------------
    tabItem(tabName = "Vuln",
            mainPanel(
              fluidRow(
                h3("How have outcomes changed for the 5 most vulnerable communities?"),
                tabsetPanel(
                  tabPanel("Percentage Change",
                           fluidRow(
                             column(12,
                                    #h1("% Positive Change"),
                                    div(style = "margin-top:5px",
                                        span(style = "font-size:15px;",
                                             htmlOutput("vulnerable_change_text"))
                                    )#end of div
                             )
                           ), #end of fluidRow
                           fluidRow(
                             column(12,
                                    div(style = "margin-top:10px",
                                        plotlyOutput("vulnerable_change",  height = "100%"))
                             )
                           ), #end of fluidrow
                           
                  ), #end of tabpanel
                  tabPanel("Value Change",
                           fluidRow(
                             column(6,
                                    selectizeInput(
                                      "vuln_indicator",
                                      "Select indicator:",
                                      unique(vulnerable_communities_data$Indicator),
                                      selected = "Attainment"
                                    ))
                           ),
                           fluidRow(
                             column(12,
                                    plotlyOutput("vulnerable_outcomes_plot", height = "100%"))
                           ))
                )
              )
            )
    ), #end of Vulnerable Communities ("Vuln")
    # My Communities ("MyCom")---------------
    tabItem(tabName = "MyCom",
            fluidPage(
              # fluidRow(
              #   span(style = "font-size:18px;",
              #        htmlOutput("my_communities_header")
              #        )
              #   ),
              # fluidRow(
              #   span(style = "font-size:14px;", 
              #        htmlOutput("my_communities_intro")
              #        )
              #   ),
              fluidRow(
                column(4, 
                       style = "padding-right:0px; padding-left:5px",
                       awesomeRadio("View",
                                    "Show:",
                                    c("All", "Most/Least 10", "Most/Least 5"),
                                    inline = TRUE,
                                    selected = "All")),
                column(1),
                column(5,
                       tags$div(
                         class = "multicol",
                         awesomeCheckboxGroup(
                           "IndiMyCom",
                           "Select indicators",
                           unique(IGZdta$Indicator),
                           selected = unique(IGZdta$Indicator))
                       )
                    ),
                column(2, 
                       actionBttn("checkboxupdate", "Update Indicators", style = "fill", color = "primary")
                       )
              ),
              fluidRow(
                conditionalPanel(condition = "input.LA1 == 'Fife'", 
                                 selectInput("Fife_SA","Select Strategic Area", 
                                             choices = c("All","Cowdenbeath", "Dunfermline", "Glenrothes", "Kirkcaldy","Levenmouth", "North East Fife", "South West Fife")))
              ),
              fluidRow(
                uiOutput("arr1"),
                column(10, 
                       plotlyOutput("community_bump_chart", width = "100%")%>% withSpinner(type = 6)
                ),
                column(1, div(tags$img(style = "max-width:120%; width:120%",src = "Arrow2.PNG")))
              )
            ) # End of fluid page
    ), #  end of My Communities ("MyCom")
    
    # Community profile ("CP")-----------------------------
    tabItem(tabName = "CP",
            fluidPage(
              tags$head(
                tags$style(HTML("
                              .multicol {
                              height:90px;
                              
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
                           plotOutput("CPplots", height = "700px") %>% withSpinner(type = 6),
                           fluidRow(
                             column(
                               6,
                               uiOutput("LineChoicesCP")
                             ),
                             column(
                               6, style = "padding-top:18px",
                               tags$img(style = "max-width:100%; max-height:fit-content",src = "ComPrflLgnd.PNG")
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
                             column(5, style = "padding-top:10px",img(style = "max-width:100%;", src = "DashedLine.PNG"))
                           )
                         )
                  )
                )  
              )
            )

    ),  #end of Community Profile ("CP")          
    # All Communities ("allCom")-------------------------
    tabItem(tabName = "allCom",
            fluidPage(
              fluidRow(
                column(
                  4,
                  selectInput(
                    "IndiAllC", 
                    "Select Indicator", 
                    unique(IGZdta$IndicatorFullName)
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
    ), #end of All Communities (allCom)
    
    # Data Zone Comparison ("Map2")----------------
    tabItem(tabName = "Map2",
            fluidPage(
              div(
                style = "margin-left:40px", 
                uiOutput("IZUI")),
              conditionalPanel("input.CPP != ' '", 
                               fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                                                    h4("Percentage of Children in Poverty"), 
                                                    h4("Average Highest Attainment"), 
                                                    h4("% Aged 16-64 Receiving Out of Work Benefits"))
                               ),
                               fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
                                                    leafletOutput("newplot"), 
                                                    leafletOutput("newplot2"), 
                                                    leafletOutput("newplot3"))), 
                               hr(style = "margin-bottom:0.3vh; margin-top:0.5vh")),
              conditionalPanel("input.CPP != 'Select a CPP'",
                               fluidRow(
                                 splitLayout(cellWidths = c("33%", "33%", "33%"),
                                             h4("Number of SIMD Crimes per 10,000 People"), 
                                             h4("Emergency Hospital Admissions per 100,000 people aged 65+"))
                               ),  
                               fluidRow(
                                 splitLayout(cellWidths = c("33%", "33%", "33%"),
                                             leafletOutput("newplot4"), 
                                             leafletOutput("newplot5")))
              )
              
            ) 
    ),  #end of Data Zone Comparison ("Map2")
    
    # About/Data Download ("DtaDL")-------------------
    tabItem(tabName = "DtaDL",
            fluidPage(
              fluidRow(h3("About this tool"), p("The CPOP tool aims to help you assess if the lives of people in your community are improving by providing a set of core measures on important life outcomes including early years, older people, safer/stronger communities, health and wellbeing, and engagement with local communities and a consistent basis for measuring outcomes and inequalities of outcome in your area."), hr()),
              fluidRow(h3("Explainer Video"), HTML('<iframe width="789" height="444" src="https://www.youtube-nocookie.com/embed/rhno_7VMX38?autoplay=0&showinfo=0&loop=1&rel=0" frameborder="0" allow="accelerometer; loop ;encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),hr()),
              fluidRow(h3("Download the Data"), p("Use these buttons to download all of the data used in this tool. Please note that much of this data is modelled and so may not match exactly with data from other sources. This data was most recently updated on 14/09/2023."),
                       downloadBttn("DLDta",label = "Download All CPP Data"),
                       downloadBttn("DLIZDta", label = "Download All Community Data", style = "fill", color = "success"),
                       hr()),
              fluidRow(h3("Methodology"), p("You can find details on the methodology used to collect and prepare all of our indicators", style = "display:inline"), a("here", href = "https://drive.google.com/file/d/1muNlJmWgWj5FA2vU1lYzQ4NMgwuH_vzh/view?usp=sharing", target = "_blank"), hr()),
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
                                                            "Scotland's Environment Web",  class="externallink", target = "_blank")),
                         #Link to Stats.gov
                         tags$li(class= "li-custom", tags$a(href="https://statistics.gov.scot/home", 
                                                            "Statistics.gov.scot",  class="externallink", target = "_blank")),
                         tags$li(class= "li-custom", tags$a(href="https://www.nomisweb.co.uk/", 
                                                            "NOMIS - Official Labour Market Statistics",  class="externallink", target = "_blank")),
                         tags$li(class= "li-custom", tags$a(href="https://stat-xplore.dwp.gov.uk/webapi/jsf/login.xhtml", 
                                                            "DWP - Stat-Xplore",  class="externallink", target = "_blank"))
                         
                         
                       ))
            )
    ) #end of About/Data Download ("DtaDL")
  ) #end of tabItems
) #end of body


#dashboardPage function-------------------------------
dashboardPage(title = "CPOP",
              dashboardHeader(
                title = tags$img(src = "Improvement Service Logo.png", style = "height:110%;margin-left:3px"),
                tags$li(
                  class = "dropdown", 
                  tags$head(
                    tags$style(HTML('#HelpButton{background-color:White;
                                     font-size: 20px;
                                     font-weight: 600
                                    }')
                    )
                  ),
                  div(style = "padding-right:20px; padding-top:5px",actionBttn("HelpButton", "Help with this page", icon = icon("question-circle", verify_fa = FALSE), style = "jelly")))
              ),
              sidebar,
              body
)
