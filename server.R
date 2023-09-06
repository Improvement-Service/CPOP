shinyServer(function(input, output, session) {

  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  
  #shiny alert pop-up------------------------
  shinyalert(title = "", 
             text = tags$div(class="header", 
                             checked=NA,
                             tags$i(class = "fa-solid fa-circle-info", style="font-size: 42px; color:#295D8A; padding: 15px 15px"),
                             tags$p("Welcome", style = "color:#295D8A; font-size: 26px"),
                             tags$br(),
                             tags$p("The Community Planning Outcomes Profiler is for tracking improvement in communities across Scotland.", style="color:#4682B4"),
                             tags$br(),
                             HTML("<p style = color:#4682B4><b>To get started </b> use the map (or the dropdown in the sidebar) to select a CPP, then use the menu on the left to explore the data.</p>"),
                             ),
             html = TRUE)

  
  #dynamic sidebar output-----------------------
  #observe FIRST instance of LA1 input, and render the sidebar menu items in two halves (this is to accommodate the
  #conditional panel (community drop down list) which is rendered between these menu items)
  observeEvent(input$LA1,{

  output$firstHalfMenu <- renderMenu({
    req(input$LA1)
    firstHalfMenuItems <- list(menuItem("CPP Over Time", tabName = "P1", icon = icon("line-chart")),
                         menuItem("Compare All CPPs", tabName = "P2", icon = icon("bar-chart")),
                         menuItem("Compare Similar CPPs", tabName = "P3", icon = icon("area-chart")),
                         menuItem("Inequality Over Time", tabName = "InQ", icon = icon("arrows-v")),
                         menuItem("Vulnerable Communities", tabName = "Vuln",icon = icon("table")),
                         menuItem("My Communities", tabName = "MyCom", icon = icon("columns")),
                         menuItem("Community Profile", tabName = "CP", icon = icon("arrow-down"))
                         )
    sidebarMenu(firstHalfMenuItems)
    })#close renderMenu (firstHalfMenu)
  
  output$secondHalfMenu <- renderMenu({
    req(input$LA1)
    secondHalfMenuItems <- list( menuItem("All Communities", tabName = "allCom", icon = icon("picture-o")),
                                 menuItem("Data Zone Comparison", tabName = "Map2", icon = icon("globe")),
                                 menuItem("About/ Data Download", tabName = "DtaDL", icon = icon("download"))
                                 )
    sidebarMenu(secondHalfMenuItems)
    })#close renderMenu (secondHalfMenu)
  
  }, once = TRUE)#close observe event
  
  #dynamically render the select a community drop down depending on whether the CP is currently selected.
  output$communityDropDown <- renderMenu({
    req(input$LA1)
    conditionalPanel(condition = 'input.tabs == "CP" && input.LA1 !== null', selectInput("CommunityCP", "Select a Community:", choices = communities_list()))
  })
  
  # "Map1" scotMap ------
    
  output$scotMap <- renderLeaflet({
    leaflet(SpPolysLA) %>%
      addTiles() %>%
      addPolygons(
        smoothFactor = 1, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~NAME,
        fillColor = "grey", 
        color = "black",
        label = SpPolysLA@data$NAME,
        highlightOptions = highlightOptions(color = "white", weight = 3,bringToFront = TRUE)
      ) 
  }) #end of scotmap
  
  # "Map1" communityMap ------
  
  output$communityMap <- renderLeaflet({
    req(input$LA1)
    sbst <- which(SpPolysIZ@data$council %in% input$LA1)
    dt <- SpPolysIZ[sbst,]
    selCls <- if(input$CBCols){clrsCB}else{clrs}
    selPls <- if(input$CBCols){
      ~HighGoodColourBinsCB(`rank_decs`)
    }else{~HighGoodColourBins(`rank_decs`)}
    topRk <- paste0("Least vulnerable - ",nrow(dt))
    cp <- leaflet(dt) %>%
      addTiles() %>%
      addLegend("bottomleft", colors = selCls,
                labels = c("Most vulnerable - 1", "","","","","",topRk),
                opacity = 1,
                title = "") %>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~InterZone, 
        fillColor = selPls, 
        color = "black"
      )
  }) #end of communityMap
  
  # "Map 1" Update selectize input ----------
  observe({
    event <- input$scotMap_shape_click
    if(is.null(event)){
      return()} 
    updateSelectizeInput(session,"LA1", label = NULL, choices = NULL, selected = event$id)
  })
  
  # Make popup appear and clear old popups
  observe({
    leafletProxy("communityMap") %>% clearPopups()
    event <- input$communityMap_shape_click
    if(is.null(event)){
      return()}
    isolate({
      showIZPopup(event$id, event$lat, event$lng)
    })
  })
  
  # "P1"  legend outputs ---------------------------------
  output$CPPLgnd <- renderText({
    txt <- input$LA1
  })
  
  output$CompLgnd <- renderText({
    txt <- input$CompLA1
  })
  
  # "P1" selected_dta_1 -------------------
  selected_dta_1 <- reactive({
    #CPP_Imp$colourscheme <- ifelse(CPP_Imp$CPP == input$LA1,"A","B")
    data <- filter(CPP_Imp, CPP %in% c(input$LA1, input$CompLA1)) %>%
      addColourSchemeColumn(CPP,input$LA1,input$CompLA1)
  })
  
  # "P1" loop to create plots -------------------  
  
  for(i in seq_along(indicators)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        req(input$LA1)
        dta <- selected_dta_1()
        # Y Axis Range for each plot, based on range of full data set
        y_rnge_dta <- subset(CPP_Imp, CPP_Imp$Indicator == indicators[my.i])
        y_min <- min(y_rnge_dta$value, na.rm = TRUE)
        y_max <- max(y_rnge_dta$value, na.rm = TRUE)
        Rnge <- y_max - y_min
        Extra <- Rnge * 0.05
        y_min <- y_min - Extra
        y_max <- y_max + Extra
        loopdata <- subset(dta, dta$Indicator == indicators[my.i])
        # set x axis labels on plots
        # need a column which stores a numeric series to be used as the break points
        # need an additional column which specifies the labels, allowing middle years to be blank
        # the numeric column is also used as a reactive reference point for setting the labels
        loopdata <- arrange(loopdata, CPP)
        loopdata <- setDT(loopdata)[, YearBreaks :=(seq(1 : length(Year))), by = CPP]
        loopdata <- setDT(loopdata)[, YearLbls :=Year, by = CPP]
        loopdata$YearLbls <- as.character(loopdata$YearLbls)
        year_breaks <- unique(loopdata$YearBreaks)
        loopdata$YearLbls[loopdata$YearBreaks > 1 & loopdata$YearBreaks < last(loopdata$YearBreaks)] <- ""
        year_labels <- filter(loopdata, CPP == input$LA1)
        year_labels <- year_labels$YearLbls
        year_labels <- gsub("20", "", year_labels)
        
        # store raw data to be used for solid line
        
        dtaRaw <- loopdata[loopdata$Type == "Raw data",]        
        
        # Cerate plot
        
        ggplot()+
          geom_line(
            data = loopdata,
            aes(
              x = YearBreaks, 
              y = value, 
              group = colourscheme, 
              colour = colourscheme, 
              linetype = "2"
            ),
            lwd = 1, show.legend = FALSE
          )+
          geom_line(data = dtaRaw,
                    aes(x = YearBreaks,
                        y = value, 
                        group = colourscheme, 
                        colour = colourscheme, 
                        linetype = "1"
                        ), 
                    lwd = 1, show.legend = FALSE
                    ) +
          scale_color_manual(values = c("red", "blue"))+
          labs(title  = indicators[my.i])+
          annotate(
            "text", 
            x = Inf, 
            y = Inf, 
            label = sprintf('\U25CF'), 
            size = 7, 
            colour = trafficLightMarkerColour(loopdata, input$LA1, input$CompLA1),
            hjust = 1, 
            vjust = 1
          )+
          scale_x_continuous(breaks = c(1: length(year_breaks)), labels = year_labels)+
          ylim(y_min, y_max)+
          theme(
            plot.title = element_text(size = 10), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour="black"),
            axis.text.x = element_text(vjust = 0.3),
            axis.text.y = element_text(size = 7),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          ) #end of ggplot()
        
      })
    })  
  } #end of plot for loop

  # "P2" comparator CP drop-down ------------------------------------------------------------------   
  output$CompSelection <- renderUI({
    selectizeInput("OtherCPP", "Select a Comparator CPP", choices =CPPNames[CPPNames!= input$LA1], 
                   options = list(
                     placeholder = "Select a CPP",
                     onInitialize = I('function() { this.setValue(""); }')))
  })
  
# "P2" Render compare CPP plots loop-----------------
  
  P2_selections_data <- reactive({
    dta <- CPP_Imp %>%
      filter( Year == RcntYear) %>%
      addColourSchemeColumn(CPP, input$LA1, input$OtherCPP)
  })

  for(i in seq_along(indicators)){
    local({
      this_i <- i
      plotnameCPP <- paste("plot_CPP", this_i, sep ="_")
      output[[plotnameCPP]] <- renderPlot({
        
    req(input$LA1)
    # #filter so that the Scotland value isn't a bar on the plot

    dtaNoScot <- P2_selections_data() %>% filter(CPP != "Scotland")
  
    #Generate plots
    indi <- indicators
    ##calculate maximum limit for y axis  
      maxAx <- max(dtaNoScot[dtaNoScot$Indicator == indi[[this_i]],5])*1.05
      minAx <- 0
      ggplot(data = filter(dtaNoScot, Indicator == indi[[this_i]])) +
        geom_bar(aes(
          x = if((first(`High is Positive?`))== "Yes"){reorder(CPP, value)}else{reorder(CPP, -value)}, 
          y = value, 
          fill = colourscheme
          ), 
          stat = "identity",
          position = "dodge",
          #colour = "black",
          width = 0.8
        ) +
        #ALTERED
        #sclFll+
        scale_fill_manual(values = c("lightblue2","red2", "green4"), breaks = c("C", "A", "B")) +
        #scale_x_discrete(label = function(x) abbreviate(x, minlength = 4))+
        scale_y_continuous(expand = c(0,0), limits = c(minAx, maxAx))+    
        guides(fill = "none") +
        ggtitle(indi[[this_i]])+
        xlab("")+
        ylab("")+
        #    {if(input$ScotCheckbox == TRUE)
        geom_hline(aes(
          yintercept = filter(P2_selections_data(), CPP == "Scotland" & Indicator == indi[[this_i]])$value
          ), colour = "navyblue", size = 1.2
        )+
          #} +
        
        theme_bw()+
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
          #axis.text.x = element_text(angle =90, hjust =1, vjust = 0),
              plot.title = element_text(face = "bold", size = 9),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
          plot.margin = unit(c(2,2,15,2),"mm"))
    })
  })
  } #end of plot loop
  
  # "P2" legend text --------------
  output$BarLA <- renderText({
    txt <- input$LA1
  })
  output$BarComp <- renderText({
    txt <- input$OtherCPP
  })
  output$BarScot <- renderText({
    txt <- "Scotland"
  })
  
  # "P3" Create Graphs for CPP similar loop ----------------------------------------------------------------
  for(i in seq_along(indicators)){
    local({
      that_i <- i
      plotnameCPPSim <- paste("plotSimCPP", that_i, sep ="_")
      output[[plotnameCPPSim]] <- renderPlot({
        req(input$LA1)
        FGroup <- filter(CPP_Imp, CPP == input$LA1)[[1,7]]
        dta <- filter(CPP_Imp, Year == RcntYear & FG %in% FGroup) %>%
          addColourSchemeColumn(CPP, input$LA1)
        #dta$colourscheme <-ifelse(dta$CPP == input$LA1,"Sel1","Other")
        #filter so that the Scotland value isn't a bar on the plot
        
        dtaNoScot <- filter(dta, CPP != "Scotland")
        
        #Generate plots
        indi <- indicators
        ##calculate maximum limit for y axis  
        ScotVal <- filter(CPP_Imp,Year == RcntYear & 
                            Indicator == indi[[that_i]] &
                            CPP == "Scotland")$value
        maxAx <- max(c(dta[dta$Indicator == indi[[that_i]],]$value, ScotVal))*1.05
        minAx <- 0
        ggplot(data = filter(dta, Indicator == indi[[that_i]])) +
          geom_bar(aes(
            x = if(first(`High is Positive?`)== "Yes"){reorder(CPP, value)}else{reorder(CPP, -value)}, 
            y = value, 
            fill = colourscheme
          ), 
          stat = "identity", 
          position = "dodge", 
          width = 0.5
          ) +
          scale_x_discrete(label = function(x) abbreviate(x, minlength = 10))+
          scale_fill_manual(values = c("lightblue2","red2"), breaks = c("C", "A")) +
          guides(fill = "none") +
          scale_y_continuous(expand = c(0,0), limits = c(minAx,maxAx))+       
          ggtitle(indi[[that_i]])+
          xlab("")+
          ylab("")+
          #     {if(input$ScotCheckbox2 == TRUE)
          geom_hline(
            aes(
              yintercept = ScotVal
            ), colour = "navyblue", size = 1.2
          )+ 
          #}+
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust =1, vjust = 0),
                plot.title = element_text(face = "bold", size = 9),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      })
    })
  } #end of "P3" for loop
  
  # "InQ" legend text --------------
  output$CompLgndInq <- renderText({
    txt <- input$InqComp
  })
  output$CPPLgndInq <- renderText({
    txt <- input$LA1
  })
   
  # "InQ" inqTable output --------------
   output$inqTbl <- function(){
    #filter dataset
    req(input$LA1)
    dd <- filter(InqDta, CouncilName %in% c(input$LA1, input$InqComp))
    dd$value <- round(dd$value,1)
    #remove year column - no longer needed once year filter is removed
    #dd <- dd[-2]
    dd <- dd %>% unite(Titles, Indicator, Year, sep = " - ")
    dd <- spread(dd, Titles, value)
    dd <- dd[c(2,1,3:10)]
    dd[2] <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    OrdCPPs <-c(input$LA1, input$InqComp)
    dd <- arrange(dd,match(CouncilName, OrdCPPs), desc(CouncilName))
    #rownames(dd) <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    #    #mutate each column to add a popver showing the year-DOESN'T WORK IN SHINY
    #   dd <- dd %>% mutate(`Child Poverty (%)` =  cell_spec(`Child Poverty (%)`,popover = spec_popover(content = dd[[1]])))
    colnames(dd)[1:2] <- c("","")
    tbl1 <- kable(dd, "html", align = "c", escape = F)%>% 
      kable_styling("basic")%>%
      row_spec(0,background = "black", color = "white", font_size = 14, 
               align = "right") %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      column_spec(2, bold = TRUE) %>%
      collapse_rows(1,valign = "middle",latex_hline = "major")%>%
      row_spec(3, extra_css = "border-top: solid 1px")  %>%
      row_spec(0, align = "c") %>%
      scroll_box(width = "100%")
    #group_rows("", 3,4, label_row_css = "background-color: black; height: 3px") 
    #group_rows("", 1,2, label_row_css = "height:1px")
    
    # row_spec(tbl1,1, hline_after = TRUE) 
  } #end of inqTbl
  
  # "InQ" InqGrp Plot --------------
  output$InqGrp <- renderPlot({
    req(input$LA1)
    DIdta <- filter(DIdta, la %in% c(input$LA1, input$InqComp)) %>% arrange(ind, match(la, c(input$LA1, input$InqComp)))
    DIdta <- DIdta[DIdta$ind != "Out of Work Benefits", ]
    indList <- unique(DIdta$ind)
    ##get the dot colour
    DIdta <- setDT(DIdta)[,Higher :=
                            abs(first(value))<abs(last(value)),
                          by = list(ind, year)]
    DIdta <- setDT(DIdta)[,IRHigher :=
                            first(ImprovementRate)<last(ImprovementRate),
                          by = list(ind, year)]
     DIdta <- DIdta %>% addColourSchemeColumn(la, input$LA1)
    ##create colourscheme
    #descText <- "These graphs will help you understand\ninequality in outcomes across the whole of the\nCPP, with 0 indicating perfect equality and\nvalues between 0 and 1 indicating that income\ndeprived people experience poorer outcomes,\n and values between -1 and 0 indicating that\nnon-income deprived people experience\npoorer outcomes."
    #DIdta$coloursch <- ifelse(DIdta$la ==input$LA1, "A", "C")
    lstDi <- lapply(1:7,FUN = function(y){
      dta <- DIdta[DIdta$ind == indList[y],]
      DDta <- filter(dta, year == last(year))
      CDot <- if_else(DDta$Higher == T && DDta$IRHigher == T, "green", if_else(DDta$Higher == F && DDta$IRHigher == F, "red", "yellow"))
      ggplot(dta, aes(x = year, y = value))+
        geom_line(data = dta[!is.na(dta$value),], aes(group = colourscheme, colour = colourscheme), size = 1)+
        ylab("")+
        xlab("")+
        annotate(
          "text", x = Inf, y = Inf,label = sprintf('\U25CF'),size = 7, 
          colour = CDot, 
          hjust = 1, 
          vjust = 1
        )+
        ggtitle(indList[y])+
        theme_bw()+
        scale_y_continuous(limits = c(-0.23,0.5))+
        scale_x_continuous(breaks = seq(2008,2020, by  =2))+
        geom_hline(yintercept = 0)+
        scale_colour_manual(breaks = c("C", "A"), values = c("blue", "red"))+
        guides(colour = "none")+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour="black"))
    })
    do.call("plot_grid", c(lstDi, ncol = 4, align = "v"))
  }) #end of InqGrp
  
  #"InQ" render inequality comparator -----------
  output$ICompUI <- renderUI({
    CPPNames <- CPPNames[CPPNames != input$LA1]
    selectInput("InqComp", "Select Comparator",
                c("Scotland",CPPNames))
  })
  
  #"Vuln" header text ------------
  output$HeaderVuln <- renderText({
    req(input$LA1)
    txt <- paste("Have outcomes for the five most vulnerable communities in",input$LA1, "improved faster or slower than the CPP as a whole?")
  })
  
  # "Vuln" VulnTable output ------------
  
  output$VulnTable <- function(){
    CPPSelected <- input$LA1
    CompleteSet <- filter(VulnComm, CPP == CPPSelected)
    CompleteSet <- CompleteSet[,-2]
    #Store which cells should be coloured
    
    CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "Faster"] <- cell_spec(CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "Faster"], color = "green")
    CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "Slower"] <- cell_spec(CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "Slower"], color = "red")
    CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "No Difference"] <- cell_spec(CompleteSet[,c(6,9,12,15,18,21,24,27)][CompleteSet[,c(6,9,12,15,18,21,24,27)] == "No Difference"], color = "black")
    
    CompleteSet[,3][CompleteSet[,3] == "Faster"] <- cell_spec(CompleteSet[,3][CompleteSet[,3] == "Faster"], color = "white", background = "green")
    CompleteSet[,3][CompleteSet[,3] == "Slower"] <- cell_spec(CompleteSet[,3][CompleteSet[,3] == "Slower"], color = "white", background = "red")
    CompleteSet[,3][CompleteSet[,3] == "No Difference"] <- cell_spec(CompleteSet[,3][CompleteSet[,3] == "No Difference"], color = "black", background = "yellow")
    
    #move name to front
    CompleteSet <- CompleteSet[c(2,28,3:27)]
    
    ##Some nice regex to change names dynamically
    names(CompleteSet)[1:3] <- c(
      "Vulnerability",
      "Name",
      "Improvement rate compared to the CPP average")
    names(CompleteSet) <-gsub(".+Change", "Improvement rate compared to the CPP average", names(CompleteSet), perl = T)
    names(CompleteSet) <-gsub("(.+_)", "", names(CompleteSet), perl = T)
    
    CompleteSet <- 
      knitr::kable(CompleteSet,"html", escape = F) %>% 
      kable_styling(bootstrap_options = c("bordered", "hover", "responsive"), 
                    font_size = 14) %>%
      row_spec(0, color = "white", background = "black") %>%
      row_spec(6, background = "aliceblue") %>%
      column_spec(1:2, color = "white", background = "black") %>%
      collapse_rows(columns = 1, valign = "middle") %>%
      add_header_above(c(
        "Community" = 2,
        "Overall Outcomes" = 1,
        "Child Poverty" = 3,
        "Crime Rate*" = 3,
        "Depopulation" = 3,
        "Early Mortality" = 3,
        "Emergency Admissions" = 3,
        "Out of Work Benefits" = 3,
        "Participation Rate" = 3,
        "Average Highest Attainment" = 3
      ),
      background = "black",
      color = "white"
      ) %>%
      footnote(symbol = "Crime data is a three year rolled average based on modelled data")%>%
      scroll_box(width = "160%") 
  } #end of VulnTable 
  
  # "MyCom" reactive object IGZBest() ----------------
  IGZBest <- reactive({
    req(input$LA1)
    
    # rankings for outcomes
    if(input$LA1 == "Fife" & input$Fife_SA != "All"){
      IGZBest <- left_join(IGZ_latest, IGZ_latest_Fife, by = c("InterZone", "Indicator")) %>% 
        filter(CPP == "Fife" & `Strategic Area` == input$Fife_SA & Indicator %in% input$IndiMyCom) %>%
        select(-CPPScore)
      IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(SAScore), by = InterZone]
      IGZBest <-setDT(IGZBest)[, CombinedTypeScore := sum(TypeScore), by = InterZone]
    }else{
      IGZBest <- filter(IGZ_latest, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
      IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(CPPScore), by = InterZone]
      IGZBest <-setDT(IGZBest)[, CombinedTypeScore := sum(TypeScore), by = InterZone]
    }
    # Filter data so that combined scores are only displayed once for each IGZ
    
    IGZBest <- setDT(IGZBest)[, FilterRef:= first(Indicator), by = InterZone]
    IGZBest <- filter(IGZBest, Indicator == FilterRef)
    IGZBest$CPPScoreRank <- rank(IGZBest$CombinedCPPScore)
    IGZBest$TypeScoreRank <- rank(IGZBest$CombinedTypeScore)
    return(IGZBest)
  }) #end of IGZBest()
  
  # "MyCom" MyCommunitiesTbl output --------

  output$MyCommunitiesTbl <- DT::renderDataTable({  
    req(input$LA1)
    IGZBest <- IGZBest()
    # rankings for improvement 
    if(input$LA1 == "Fife" & input$Fife_SA != "All"){
      IGZImprovement <- left_join(IGZ_change, IGZ_change_Fife, by = c("InterZone", "Indicator")) %>% 
        filter(CPP == "Fife" & `Strategic.Area` == input$Fife_SA & Indicator %in% input$IndiMyCom) %>%
        select(-CPPChangeScore)
      IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(SAChangeScore), by = InterZone]
      IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    }else{
      IGZImprovement <- filter(IGZ_change, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
      IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(CPPChangeScore), by = InterZone]
      IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    }
    # Filter data so that combined scores are only displayed once for each IGZ
    
    IGZImprovement <- setDT(IGZImprovement)[, FilterRef := first(Indicator), by = InterZone]
    IGZImprovement <-filter(IGZImprovement, Indicator == FilterRef)
    
    IGZImprovement$CPPChangeRank <- rank(IGZImprovement$CombinedCPPChangeScore)
    IGZImprovement$TypeChangeRank <- rank(IGZImprovement$CombinedTypeChangeScore)
    
    # Split Data into 4 individual DataTables for each ranking, then combine into 1 table
    
    Column1 <- select(IGZBest, c(InterZone_Name, CPPScoreRank)) %>% 
      arrange(CPPScoreRank) 
    colnames(Column1)[1] <- "Variable1"
    
    Column2 <- select(IGZBest, c(InterZone_Name, TypeScoreRank)) %>%
      arrange(TypeScoreRank)
    colnames(Column2)[1] <- "Variable2"
    
    Column3 <- select(IGZImprovement, c(InterZone_Name, CPPChangeRank)) %>%
      arrange(CPPChangeRank)
    colnames(Column3)[1] <- "Variable3"
    
    Column4 <- select(IGZImprovement, c(InterZone_Name, TypeChangeRank)) %>%
      arrange(TypeChangeRank)
    colnames(Column4)[1] <- "Variable4"
    
    MyCommunitiesDta <- cbind(Column1, Column2, Column3, Column4) %>%
      select(c(-CPPScoreRank, -TypeScoreRank, -CPPChangeRank, -TypeChangeRank))
    
    # Calculate References for Colours
    
    NoIGZ      <- nrow(MyCommunitiesDta)
    NoIGZ      <- as.numeric(NoIGZ)
    Clrs       <- if(input$CBCols){ifelse(NoIGZ<9,NoIGZ,9)}else{ifelse(NoIGZ < 11,NoIGZ,11)}
    groupings  <- round(NoIGZ/Clrs)
    Number_seq <- rep(1:Clrs, each = groupings)
    
    # Check the length of this colour sequence to determine 
    # whether more needs to be added or some need to be removed
    
    length_seq <- length(Number_seq)
    Diff_seq <- NoIGZ - length_seq
    
    # if difference is negative have a smaller number within each grouping
    if(Diff_seq < 0) {groupings <- groupings -1}
    
    # Create the number sequence again on this basis
    
    Number_seq2 <- rep(1:Clrs, each = groupings)
    length_seq2 <- length(Number_seq2)
    Diff_seq2 <- NoIGZ - length_seq2
    
    # distribute a roughly equal proportion of the colours
    
    extra <- seq.int(from = 2, to = Clrs, length.out = Diff_seq2)
    extra <- round(extra)
    
    Complete_seq <- c(Number_seq2,extra)
    Complete_seq <- sort(Complete_seq)
    MyCommunitiesDta$Helper1 <- Complete_seq
    
    # colours for the remaining columns
    # seperate out the helper column then join this back up, matching with the other columns so that 
    # the colours take on the order of these variables
    
    colours2 <- MyCommunitiesDta[,c(1,5)]
    colnames(colours2) <- c("Variable2", "Helper2")
    MyCommunitiesDta <- join(MyCommunitiesDta, colours2, by = "Variable2")
    
    colours3 <- MyCommunitiesDta[,c(1,5)]
    colnames(colours3) <- c("Variable3", "Helper3")
    MyCommunitiesDta <- join(MyCommunitiesDta, colours3, by = "Variable3")
    
    colours4 <- MyCommunitiesDta[,c(1,5)]
    colnames(colours4) <- c("Variable4", "Helper4")
    MyCommunitiesDta <- join(MyCommunitiesDta, colours4, by = "Variable4")
    
    #Store unique colour reference to use as intervals in styling
    
    Store_unique1 <- unique(MyCommunitiesDta$Helper1)
    Store_unique2 <- unique(MyCommunitiesDta$Helper2) %>% sort
    Store_unique3 <- unique(MyCommunitiesDta$Helper3) %>% sort
    Store_unique4 <- unique(MyCommunitiesDta$Helper4) %>% sort
    
    if(input$CBCols){ColourPal <- rev(brewer.pal(Clrs,"YlGnBu"))}else{ColourPal <- brewer.pal(Clrs,"RdYlGn")}
    CPPName <- input$LA1
    
    Container1 <- paste(
      "Within ", 
      CPPName, 
      " which communities have the poorest outcomes?")
    Container2 <- paste(
      "Compared to other, similar communities, do those in ", 
      CPPName, 
      " fare better or worse than expected?")
    Container3 <- paste(
      "Within ", 
      CPPName, 
      " which communities have improved the least?")
    Container4 <- paste(
      "Within ", 
      CPPName, 
      "which communities have improved the least relative to other similar communities?")
    
    # add 4 empty columns so that there is space in between each column in the table
    
    MyCommunitiesDta$empt1 <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,9,2,3,4,5,6,7,8)]
    
    MyCommunitiesDta$empt2 <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,2,3,10,4,5,6,7,8,9)]
    
    MyCommunitiesDta$empt3 <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,2,3,4,5,11,6,7,8,9,10)]
    
    colnames(MyCommunitiesDta)[c(2,4,6)] <- ""
    
    # Store values of the colours which need to have white text - including colourblind option
    
    WhiteTxt <- if(input$CBCols){c(head(Store_unique1,2))}else{c(head(Store_unique1,2),tail(Store_unique1,2))}
    TxtValue <- Store_unique1
    TxtValue <- if_else(TxtValue %in% WhiteTxt, "White", "Black")
    
    # Allow table to be split into top/bottom 10 and top/bottom 5
    
    Top10Rows <- if_else(
      NoIGZ < 20,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) + 0.5
      ),
      10
    )
    
    Bottom10Rows <- if_else(
      NoIGZ < 20,
      if_else(
        (NoIGZ %% 2) == 0,
        NoIGZ / 2,
        (NoIGZ / 2) - 0.5 
      ),
      10
    )
    
    Top10 <- head(MyCommunitiesDta, Top10Rows)
    Top10 <- Top10 %>% add_row() %>% add_row()
    names(Top10)[2] <-""
    names(Top10)[4] <-""
    names(Top10)[6] <-""
    Bottom10 <- tail(MyCommunitiesDta, Bottom10Rows)
    TopBottom10 <- rbind(Top10, Bottom10)
    
    Top5Rows <- if_else(
      NoIGZ < 10,
      if_else(
        (NoIGZ %% 2) == 0,
        NoIGZ / 2, 
        (NoIGZ / 2) + 0.5
      ),
      5
    )
    
    Bottom5Rows <- if_else(
      NoIGZ < 10,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) - 0.5 
      ),
      5
    )
    
    Top5 <- head(MyCommunitiesDta,Top5Rows)
    Top5 <- Top5 %>% add_row() %>% add_row()
    names(Top5)[2] <-""
    names(Top5)[4] <-""
    names(Top5)[6] <-""
    Bottom5 <- tail(MyCommunitiesDta, Bottom5Rows)
    TopBottom5 <- rbind(Top5, Bottom5)
    
    
    Display <- input$View
    if(Display == "Top/bottom 10") {MyCommunitiesDta <- TopBottom10}
    if(Display == "Top/bottom 5") {MyCommunitiesDta <- TopBottom5}
    
    # HTML to allow column headers to span multiple columns
    
    sketch = htmltools::withTags(table(
      thead(
        tr(
          th(class = 'dt-center', colspan = 4, 'Outcomes'),
          th(class = 'dt-center', colspan = 3, 'Improvement')
        ),
        tr(
          lapply(c(Container1, "", Container2, "", Container3, "", Container4),th)
        )
      )
    ))
    
    # Create table
    
    datatable(
      MyCommunitiesDta, 
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(7,8,9,10)),
          list(width = '400px', targets = c(0,2,4,6))
        ),
        pageLength = 136, 
        dom = "t", 
        ordering = F
      ),
      container = sketch,
      class = 'compact',
      rownames = FALSE,
      selection = 'none'
    )%>%
      formatStyle(columns = 1, valueColumns = 8 ,backgroundColor = styleEqual(Store_unique1,ColourPal))%>%
      formatStyle(columns = 3, valueColumns = 9 ,backgroundColor = styleEqual(Store_unique2,ColourPal))%>%
      formatStyle(columns = 5, valueColumns = 10,backgroundColor = styleEqual(Store_unique3,ColourPal))%>%
      formatStyle(columns = 7, valueColumns = 11,backgroundColor = styleEqual(Store_unique4,ColourPal))%>%
      formatStyle(columns = 1, valueColumns = 8, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 3, valueColumns = 9, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 5, valueColumns = 10, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 7, valueColumns = 11, color = styleEqual(Store_unique1,TxtValue))
  }) #end of MyCommunitiesTbl
  
  # "MyCom" arrows  ------------
  output$arr1 <- renderUI({
    if(input$CBCols){
      column(1,div(style = "padding-left:0px; float:left",tags$img(style = "max-width:120%",src = "CBArrow1.PNG")))
    }else{
      column(1,div(style = "padding-left:0px; float:left",tags$img(style = "max-width:120%",src = "Arrow1.PNG")))
    }
  })
  output$arr2 <- renderUI({
    if(input$CBCols){
      column(width = 2, style = "padding-left:2px;padding-right:2px;z-index:1;",tags$img(style = "max-width:120%",src = "CBArrow3.PNG"))
    }else{
      column(width = 2, style = "padding-left:2px;padding-right:2px;z-index:1;",tags$img(style = "max-width:120%", src = "Arrow3.PNG"))
    }
  })
  
  # "MyCom" Value box for community progress My Communities---------------------
  output$comProgressBox <- renderValueBox({
    IGZBest <- IGZBest()
    pBetter <- round((sum(IGZBest$CombinedTypeScore>0)/nrow(IGZBest))*100,0)
    bCol <- if(pBetter <50) {"red"}else{"green"}
    valueBox(
      paste0(pBetter, "%"), "Communities Performing Better than Expected", icon = icon("percent"),
      color = bCol, width = NULL
    )
  })
  
  #"CP" communities_list()--------------------
  #reactive ex-pression to update communities in sidebar drop down (CommunityCP) when LA1 input is changed
  communities_list <- eventReactive(input$LA1, {
    commCP_dropdown_data <- filter(IGZdta, CPP == input$LA1)
    commCP_dropdown_options <- sort(unique(commCP_dropdown_data$InterZone_Name))
    return(commCP_dropdown_options)
  })#end event reactive
  
  # "CP" reactive object LineChoiceDta() -------------------
  LineChoiceDta <- reactive({
    req(input$LA1)
    req(input$CommunityCP)
    # need to filter to selected CPP to avoid cases where the 
    #IGZ name has a duplicate in another CPP
    Community            <- filter(IGZdta, InterZone_Name == input$CommunityCP& CPP == input$LA1)
    Community$Identifier <- input$CommunityCP
    Community$ColourRef  <- "A"
    Community$Colours    <- "red"
    Community            <- select(
      Community, c(-InterZone, -InterZone_Name, -CPP, -Typology_Group, -Typology_Name, -IndicatorFullName) 
    )
    
    Indicators    <- unique(IGZdta$Indicator)
    LA            <- filter(CPPdta, CPP == input$LA1 & Indicator %in% Indicators)
    LA$Identifier <- input$LA1
    LA$ColourRef  <- "B"
    LA$Colours    <- "green"
    LA            <- select(LA, c(-CPP, -FG,-IndicatorFullName))
    
    Indicators          <- unique(IGZdta$Indicator)
    Scotland            <- filter(CPPdta, CPP == "Scotland" & Indicator %in% Indicators)
    Scotland$Identifier <- "Scotland"
    Scotland$ColourRef  <- "C"
    Scotland$Colours    <- "blue"
    Scotland            <- select(Scotland, c(-CPP, -FG,-IndicatorFullName))
    
    IGZsubset         <- filter(IGZdta, InterZone_Name == input$CommunityCP& CPP == input$LA1)
    Typology          <- first(IGZsubset$Typology_Group)
    GrpAv             <- filter(IGZdta, Typology_Group == Typology)
    GrpAv <- ddply(GrpAv,. (Indicator, Year), transform, GrpAver = mean(value))
    GrpAv              <- filter(GrpAv, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    GrpAv              <- select(GrpAv, -value)
    colnames(GrpAv)[10] <- "value"
    GrpAv$Identifier   <- "Group Average"
    GrpAv$ColourRef    <- "D"
    GrpAv$Colours      <- "orange"
    GrpAv              <- select(
      GrpAv, c(-InterZone, -InterZone_Name, -CPP, -Typology_Group, -Typology_Name,-IndicatorFullName)
    )
    
    SimComm            <- IGZdta
    SimComm$InterZone_Name <- paste(SimComm$CPP, "-",SimComm$InterZone_Name)
    SimComm            <- filter(SimComm, InterZone_Name == input$ChoiceAddComm)
    SimComm$Identifier <- "Similar Community"
    SimComm$ColourRef  <- "E"
    SimComm$Colours    <- "purple"
    SimComm            <- select(
      SimComm, c(-InterZone, -InterZone_Name, -CPP, -Typology_Group, -Typology_Name,-IndicatorFullName))
    
    if(input$ChoiceAddComm == 0){
      (LineChoiceDta <- rbind(Community, LA, Scotland, GrpAv))
    }else{
      (LineChoiceDta <- rbind(Community, LA, Scotland, GrpAv, SimComm))
    }
  })#end of LineChoiceDta()
  
  #"CP" descrip--------
  output$Descrip <- renderText({
    req(input$LA1)
    IGZsubset <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    txt <- first(IGZsubset$Typology_Name)
    txt <- paste("Group Description: ", txt)
  })
  
  #"CP" GrpSize ----------
  
  output$GrpSize <- renderText({
    req(input$LA1)
    IGZsubset <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    Typology <- first(IGZsubset$Typology_Group)
    Indi <- first(IGZsubset$Indicator)
    Yr <- first(IGZsubset$Year)
    TypeSubset <- filter(
      IGZdta, 
      Typology_Group == Typology & 
        Indicator == Indi & 
        Year == Yr)
    Size <- nrow(TypeSubset) -1
    txt <- paste(Size, " other, similar communities in this group")
  })
  
  #"CP" CommunityProfileTbl --------------

  output$CommunityProfileTbl <- DT::renderDataTable({
    req(input$LA1, input$CommunityCP)
    IGZsubset <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    Typology <- first(IGZsubset$Typology_Group)
    
    # Rankings for Outcomes
    
    IGZBest <- filter(
      IGZ_latest, 
      Typology_Group == Typology & 
        Indicator %in% input$IndiCP
    )
    
    IGZBest <- setDT(IGZBest)[,CombinedTypeScore := (sum(TypeScore)), by = InterZone]
    IGZBest <- setDT(IGZBest)[,FilterRef := (first(Indicator)), by = InterZone]
    IGZBest <- filter(IGZBest, Indicator == FilterRef)
    IGZBest$TypeScoreRank <- rank(IGZBest$CombinedTypeScore)
    
    # Concatenate CPP Names with Community Names
    
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, InterZone_Name = paste(CPP, "-",InterZone_Name))
    IGZBest$InterZone_Name <-  paste(IGZBest$CPP, "-",IGZBest$InterZone_Name)
    # Rankings for Improvement
    
    IGZImprovement <- filter(
      IGZ_change, 
      Typology_Group == Typology & 
        Indicator %in% input$IndiCP
    )
    
    IGZImprovement <- setDT(IGZImprovement)[, CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    IGZImprovement <- setDT(IGZImprovement)[,FilterRef := first(Indicator), by = InterZone]
    IGZImprovement <-filter(IGZImprovement, Indicator == FilterRef)
    IGZImprovement$TypeChangeRank <- rank(IGZImprovement$CombinedTypeChangeScore)
    
    #Concatenate CPP Names with Community Names
    
    IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, InterZone_Name = paste(CPP, "-",InterZone_Name))
    
    # Split Data into individual DataTables for each ranking, then combine
    
    Column1 <- select(IGZBest, c(InterZone_Name, TypeScoreRank)) %>%
      arrange(TypeScoreRank)
    colnames(Column1)[1] <- "Variable1"
    
    Column2 <- select(IGZImprovement, c(InterZone_Name, TypeChangeRank)) %>%
      arrange(TypeChangeRank)
    colnames(Column2)[1] <- "Variable2"
    
    CommunityProfileDta <- cbind(Column1, Column2) %>%
      select(c(-TypeScoreRank, -TypeChangeRank))
    
    # Calculate References for Colours
    
    NoIGZ      <- nrow(CommunityProfileDta)
    NoIGZ      <- as.numeric(NoIGZ)
    Clrs       <- if(input$CBCols){ifelse(NoIGZ<9,NoIGZ,9)}else{ifelse(NoIGZ < 11,NoIGZ,11)}
    groupings  <- round(NoIGZ / Clrs)
    Number_seq <- rep(1:Clrs, each = groupings)
    
    # Check the length of this colour sequence to determine 
    # whether more needs to be added or some need to be removed
    
    length_seq <- length(Number_seq)
    Diff_seq <- NoIGZ - length_seq
    
    # if difference is negative have a smaller number within each grouping
    
    if(Diff_seq < 0) {groupings <- groupings -1}
    
    # Create the number sequence again on this bases
    
    Number_seq2 <- rep(1:Clrs, each = groupings)
    length_seq2 <- length(Number_seq2)
    Diff_seq2   <- NoIGZ - length_seq2
    
    # distribute a roughly equal proportion of the colours
    
    extra <- seq.int(from = 2, to = Clrs, length.out = Diff_seq2)
    extra <- round(extra)
    
    Complete_seq <- c(Number_seq2,extra)
    Complete_seq <- sort(Complete_seq)
    CommunityProfileDta$Helper1 <- Complete_seq
    
    # colours for the remaining columns
    # seperate out the helper column then join this back up, matching with 
    #the other columns so that the colours take on the order of these variables
    
    colours2 <- CommunityProfileDta[,c(1,3)]
    colnames(colours2) <- c("Variable2", "Helper2")
    CommunityProfileDta <- join(CommunityProfileDta, colours2, by = "Variable2")
    
    # Store unique colour reference to use as intervals in styling
    
    Store_unique1 <- unique(CommunityProfileDta$Helper1)
    Store_unique2 <- unique(CommunityProfileDta$Helper2) %>% sort
    
    if(input$CBCols){ColourPal <- rev(brewer.pal(Clrs,"YlGnBu"))}else{ColourPal <- brewer.pal(Clrs,"RdYlGn")}
    CPPName <-  input$LA1
    
    # determine which IGZ should be bold
    
    Community <- input$CommunityCP
    Community <- paste(CPPName, "-", Community)
    CommunityProfileDta$Community <- Community
    
    CommunityProfileDta$Community1 <- if_else(
      CommunityProfileDta$Variable1 == CommunityProfileDta$Community,
      "Yes",
      "No"
    )
    
    CommunityProfileDta$Community2 <- if_else(
      CommunityProfileDta$Variable2 == CommunityProfileDta$Community,
      "Yes",
      "No"
    )
    
    CommunityProfileDta <- select(CommunityProfileDta, -Community)
    
    fontlevels <- c("No", "Yes")
    fontvalues <- c('normal', 'bold')
    bordervalues <- c("",'2px solid black')
    
    #Rename variables
    colnames(CommunityProfileDta)[1] <- paste(
      "How does the selected community in  ", 
      CPPName, 
      " compare to similar communities in Scotland?"
    )
    
    colnames(CommunityProfileDta)[2] <- paste(
      "How does the improvement rate of the selected community in ", 
      CPPName, 
      " compare to similar communities in Scotland?"
    )
    
    # add empty column so that there is space between the columns in the table
    
    CommunityProfileDta$empt1 <- NA
    CommunityProfileDta <- CommunityProfileDta[,c(1,7,2,3,4,5,6)]
    colnames(CommunityProfileDta)[2] <- ""
    
    # Store values of the colours which need to have white text
    
    WhiteTxt <- if(input$CBCols){c(head(Store_unique1,2))}else{c(head(Store_unique1,2),tail(Store_unique1,2))}
    TxtValue <- Store_unique1
    TxtValue <- if_else(TxtValue %in% WhiteTxt, "White", "Black")
    
    # Allow table to be split into top/bottom 10 and top/bottom 5
    
    Top10Rows <- if_else(
      NoIGZ < 20,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) + 0.5
      ),
      10
    )
    
    Bottom10Rows <- if_else(
      NoIGZ < 20,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) -0.5
      ),
      10
    )
    
    Top10 <- head(CommunityProfileDta,Top10Rows)
    Top10 <- Top10 %>% add_row() %>% add_row()
    names(Top10)[2] <-""
    Bottom10 <- tail(CommunityProfileDta, Bottom10Rows)
    TopBottom10 <- rbind(Top10, Bottom10)
    colnames(TopBottom10)[2] <-""
    
    Top5Rows <- if_else(
      NoIGZ < 10,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) +0.5 
      ),
      5
    )
    
    Bottom5Rows <- if_else(
      NoIGZ < 10,
      if_else(
        (NoIGZ %% 2) == 0, 
        NoIGZ / 2, 
        (NoIGZ / 2) -0.5 
      ),
      5
    )
    
    Top5 <- head(CommunityProfileDta,Top5Rows)
    Top5 <- Top5 %>% add_row() %>% add_row()
    Bottom5 <- tail(CommunityProfileDta, Bottom5Rows)
    names(Top5)[2] <-""
    TopBottom5 <- rbind(Top5, Bottom5)
    colnames(TopBottom5)[2] <-""
    
    Display <- input$ViewCP
    if(Display == "Top/bottom 10"){CommunityProfileDta <- TopBottom10}
    if(Display == "Top/bottom 5"){CommunityProfileDta <- TopBottom5}
    
    # Create table
    
    datatable(
      CommunityProfileDta, 
      options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(3,4,5,6)),
          list(width = '400px', targets = c(0,2))
        ),
        pageLength = 136, 
        dom = "t", 
        ordering = F
      ),
      class = 'compact',
      rownames = FALSE,
      selection = 'none'
    )%>%
      formatStyle(columns = 1, valueColumns = 4 ,backgroundColor = styleEqual(Store_unique1,ColourPal))%>%
      formatStyle(columns = 3, valueColumns = 5 ,backgroundColor = styleEqual(Store_unique2,ColourPal))%>%
      formatStyle(columns = 1, valueColumns = 4, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 3, valueColumns = 5, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 1, valueColumns = 6, fontWeight = styleEqual(fontlevels,fontvalues))%>%
      formatStyle(columns = 3, valueColumns = 7, fontWeight = styleEqual(fontlevels,fontvalues))%>%
      formatStyle(columns = 1, valueColumns = 6, border = styleEqual(fontlevels,bordervalues))%>%
      formatStyle(columns = 3, valueColumns = 7, border = styleEqual(fontlevels,bordervalues))
  }) #end of CommunityProfileTbl
  
  # "CP" LineChoicesCP --------------
  output$LineChoicesCP <- renderUI({
    req(input$LA1, input$CommunityCP)
    Choices <- c(input$CommunityCP, input$LA1, "Scotland", "Group Average", "Similar Community")
    awesomeCheckboxGroup(
      "ChoicesCP", 
      "Select lines to plot", status = "danger",
      Choices, selected = c(input$CommunityCP, input$LA1, "Scotland", "Group Average"))
  })
  
  # "CP" AddComm ---------------
  output$AddComm <- renderUI({
    Comm <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    Group <- Comm$Typology_Group[1]
    Options <- filter(IGZdta, Typology_Group == Group)
    Options <- filter(Options, InterZone_Name != input$CommunityCP | CPP != input$LA1)
    Options$InterZone_Name <-  paste(Options$CPP, "-",Options$InterZone_Name)
    pickerInput(
      "ChoiceAddComm", 
      "", 
      choices = unique(Options$InterZone_Name), options(size = 10)
    )
  })
  
  #"CP" toggle similar community  ------------------
  observe({
    shinyjs::toggle(id = "AddComm", condition = ("Similar Community" %in% input$ChoicesCP))
  })
  
#WHAT DOES THIS DO?-------------------
  outputOptions(output, 'AddComm', suspendWhenHidden = FALSE)

  
  # "CP" Create plot outputs ------------------------
  IndicatorsCP <- unique(IGZdta$Indicator)
  nIndis <- length(IndicatorsCP)
  
  output$CPplots<- renderPlot({
    req(input$LA1)
    req(input$CommunityCP)
    plots <- list()
    plots <- lapply(1:nIndis, FUN = function(.x){
      
      LineChoiceDta <- LineChoiceDta()
      
      # Y axis 
      
      y_rnge_dta <- subset(LineChoiceDta, LineChoiceDta$Indicator == IndicatorsCP[.x])
      y_min <- min(y_rnge_dta$value, na.rm = TRUE)
      y_max <- max(y_rnge_dta$value, na.rm = TRUE)
      Rnge <- y_max - y_min
      Extra <- Rnge * 0.05
      y_min <- y_min - Extra
      y_max <- y_max + Extra
      
      # set x axis labels on plots
      # need a column which stores a numeric series to be used as the break points
      # need an additional column which specifies the labels, allowing middle years to be blank
      # the numeric column is also used as a reactive reference point for setting the labels
      
      LineChoiceDta$YearLabels <- LineChoiceDta$Year
      LineChoiceDta$YearLabels <- if_else(
        LineChoiceDta$Year == FrstYear,
        LblFrst,
        if_else(
          LineChoiceDta$Year == RcntYear, 
          LblRcnt,
          if_else(
            LineChoiceDta$Year == ProjYear,
            LblProj,
            ""
          )
        )
      )
      
      
      LineChoiceDta <- setDT(LineChoiceDta)[,YearPoints := seq(1 : length(Year)), by = list(Indicator, Identifier) ]
      
      # filter data to selection and individual indicator
      
      LineChoiceDta <- filter(LineChoiceDta, Identifier %in% input$ChoicesCP)
      
      if(input$ProjectionsCP == "No"){LineChoiceDta <- filter(
        LineChoiceDta, Type != "Projected")} 
      
      loopdata <- filter(LineChoiceDta, Indicator == IndicatorsCP[.x])
      
      ColourRefPnts <- unique(loopdata$ColourRef)
      LineColours <- unique(loopdata$Colours)
      
      YPoints <- unique(loopdata$YearPoints)
      YPoints <- as.numeric(YPoints)
      FilterRef <- first(loopdata$Identifier)
      YLabels <- filter(loopdata, Identifier == FilterRef)
      YLabels <- YLabels$YearLabels
      
      # Seperarate projected data so this can be plotted seperately
      
      DashedLine <- loopdata
      SolidLine <- filter(loopdata, Type != "Projected")
      
      
      # Create Plot
      
      ggplot()+
        geom_line(
          data = DashedLine, 
          aes(
            x = YearPoints, 
            y = value, 
            group = ColourRef, 
            colour = ColourRef, 
            linetype = "2"
          ),
          lwd = 1, 
          show.legend = FALSE
        )+
        geom_line(
          data = SolidLine, 
          aes(
            x = YearPoints, 
            y = value, 
            group = ColourRef, 
            colour = ColourRef, 
            linetype = "1"
          ),
          lwd = 1, 
          show.legend = FALSE
        )+
        ggtitle(IndicatorsCP[.x])+
        scale_colour_manual(breaks = ColourRefPnts, values = LineColours)+
        scale_x_continuous(breaks = c(1: length(YPoints)), labels = YLabels)+
        ylim(y_min, y_max)+
        theme(
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour="black"),
          axis.text.x = element_text(vjust = 0.3),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank()
        )
    })
    
    do.call("plot_grid", c(plots, ncol = 2, align = "v"))
  }) # end of CPplots
  
  # "allCom" legend text ------------------
  
  output$CommLgnd <- renderText({
    txt <- "Community"
  })
  
  output$CPPLgnd2 <- renderText({
    txt <- input$LA1
  })
  
  output$ScotLgnd <- renderText({
    txt <- "Scotland"
  })
  
  # "allCom" plot variable ------------------
  myheight <- function(){
    req(input$LA1)
    nrow(unique(IGZdta[IGZdta$CPP == input$LA1,"InterZone_Name"]))*60
  }
  
  # "allCom"  AllCPlots ---------------------
  output$AllCPlots <- renderPlot({
    req(input$LA1)
    
    dta <- IGZdta[IGZdta$CPP == input$LA1 & 
                    IGZdta$IndicatorFullName == input$IndiAllC &
                    IGZdta$Type != "Projected",
                  c(2,8,9)
    ]
    
    nComs <- length(unique(dta$InterZone_Name))
    comList <- unique(dta$InterZone_Name)%>% sort
    dta2 <- CPPdta[CPPdta$CPP %in% input$LA1 & 
                     CPPdta$IndicatorFullName == input$IndiAllC & 
                     CPPdta$Type != "Projected",
                   c(1,4,5)
    ]
    dta3 <- CPPdta[CPPdta$CPP %in% "Scotland"& 
                     CPPdta$IndicatorFullName == input$IndiAllC &
                     CPPdta$Type != "Projected",
                   c(1,4,5)
    ]
    
    colnames(dta2) <- colnames(dta)
    colnames(dta3) <- colnames(dta)
    dta            <- rbind(dta, dta2, dta3) %>%
      addColourSchemeColumn(InterZone_Name, input$LA1, "Scotland")
    
    # Y axis range 
    y_min <- min(dta$value, na.rm = TRUE)
    y_max <- max(dta$value, na.rm = TRUE)
    Rnge <- y_max - y_min
    Extra <- Rnge * 0.05
    y_min <- y_min - Extra
    y_max <- y_max + Extra
    
    yrs <- c(dta$Year[[1]], dta$Year[[length(dta$Year)]])
    yrs2 <- gsub("20", "", yrs)
    
    # lapply to generate plots
    
    plts <- list()
    plts <-lapply(1:nComs, FUN = function(.x){
      ggplot(
        data = dta[dta$InterZone_Name %in% c(comList[.x], input$LA1, "Scotland"),])+
        geom_line(
          aes(x = Year, y = value, group = colourscheme, colour = colourscheme), 
          size = 1
        )+
        theme_bw()+
        ggtitle(comList[.x])+
        theme(axis.text.x =  element_text(angle = 90, vjust = 0, hjust = 1))+
        ylab("")+
        xlab("")+
        scale_x_discrete(breaks = yrs, labels = yrs2, expand = c(0.01,0.01))+
        ylim(y_min, y_max)+
        scale_color_manual(
          breaks = c("C", "A", "B"), 
          values = c("red", "green4","blue")
        )+
        guides(colour = "none")+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    })
    do.call("plot_grid", c(plts, ncol = 4))
  }, 
  height = myheight) #end of AllCPlots
  
  # "Map2" select intermediate geography -----------------
  output$IZUI <- renderUI({
    selectizeInput(
      "IZ", 
      "", 
      choices = sort(unique(CPPMapDta[CPPMapDta$council == input$LA1, 11])),
      options = list(
        placeholder = "Select a Community",
        onInitialize = I('function() { this.setValue(""); }'))
    )
  })
  
  # "Map2" plydata() for leaflet outputs --------------
  plydata <- reactive({
    desIZ <- which(CPPMapDta$council %in% input$LA1 & CPPMapDta$IZname %in% input$IZ)
    IZ_dzs <- SpPolysDF[desIZ,]
  })
  
  # "Map2" leaflet outputs (newplot 1 -5) ---------------------
  
  output$newplot<-renderLeaflet({
    mapCols <- if(input$CBCols){~LowGoodColourBinsCB(`povDecs`)}else{~LowGoodColourBins(`povDecs`)}
    p <- leaflet(plydata()) %>%# addProviderTiles("OpenStreetMap.HOT")%>% #Humanitarian OpenStreetMap if desired
      addTiles()%>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.6,
        layerId = ~DataZone, 
        fillColor = mapCols, 
        color = "black"
      )
    return(p)
  })
  
  output$newplot2 <- renderLeaflet({
    mapCols <- if(input$CBCols){~HighGoodColourBinsCB(`tariffDecs`)}else{~HighGoodColourBins(`tariffDecs`)}
    
    p <- leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~DataZone, 
        fillColor = mapCols,  
        color = "black"
      )
    
    return(p)
  })
  
  output$newplot3 <- renderLeaflet({
    mapCols <- if(input$CBCols){~LowGoodColourBinsCB(`benDecs`)}else{~LowGoodColourBins(`benDecs`)}
    p <- leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~DataZone, 
        fillColor = mapCols, 
        color = "black"
      )
    
    return(p)
  })
  
  output$newplot4 <- renderLeaflet({
    mapCols <- if(input$CBCols){~LowGoodColourBinsCB(`crimeDecs`)}else{~LowGoodColourBins(`crimeDecs`)}
    p <- leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~DataZone, 
        fillColor = mapCols, 
        color = "black"
      )
    
    return(p)
  })
  
  output$newplot5 <- renderLeaflet({
    mapCols <- if(input$CBCols){~LowGoodColourBinsCB(`admisDecs`)}else{~LowGoodColourBins(`admisDecs`)}
    p <- leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(
        smoothFactor = 0.5, 
        weight = 1.5, 
        fillOpacity = 0.7,
        layerId = ~DataZone, 
        fillColor = mapCols, 
        color = "black"
      )
    
    return(p)
  })
  
  # "Map2" observe events to show/clear pop-ups------------
  
  observe({
    leafletProxy("newplot") %>% clearPopups()
    event <- input$newplot_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "Children in Poverty", "newplot")
    })
  })
  
  observe({
    leafletProxy("newplot2") %>% clearPopups()
    event <- input$newplot2_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "Average Highest Attainment", "newplot2")
    })
  })

  observe({
    leafletProxy("newplot3") %>% clearPopups()
    event <- input$newplot3_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "Out of Work Benefits", "newplot3")
    })
  })
  
  observe({
    leafletProxy("newplot4") %>% clearPopups()
    event <- input$newplot4_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "SIMD Crimes", "newplot4")
    })
  })
  
  observe({
    leafletProxy("newplot5") %>% clearPopups()
    event <- input$newplot5_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "Emergency Admissions", "newplot5")
    })
  })
  
  # About / Download data ----------------------------
  #CPP data
  output$DLDta <- downloadHandler(
    filename = paste("CPP Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/CPP Data - Aug 22.zip", con)
    },
    contentType = "application/zip"
  )
  
  #IZ data
  output$DLIZDta <- downloadHandler(
    filename = paste("IGZ Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/IGZ Data - Aug 22.zip", con)
    },
    contentType = "application/zip"
  )
  
  # code for 'Help' pages ----------------------------------------------

  # Help "Map1"----------------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "Map1")
    {showModal(modalDialog(
      title = "Community Map - Page 1 of 2",
      fluidRow(
      tags$img(src = "MapHelp1.PNG")),
      fluidRow(
        column(10),
        column(2,actionButton("Map1P2", "Next Page"))
      ),
      size = "l"))}
  })
  
  observeEvent(input$Map1P2,{
    showModal(
      modalDialog(
        title = "Community Map - Page 2 of 2", 
        fluidRow(tags$img(src = "MapHelp2.PNG")),
        fluidRow(actionButton("Map1P1", "Previous Page")),
        size = "l"
      )
    )
  })
  
  observeEvent(input$Map1P1,{showModal(modalDialog(
    title = "Community Map - Page 1 of 1",
    fluidRow(
      tags$img(src = "MapHelp1.PNG")),
    fluidRow(
      column(10),
      column(2,actionButton("Map1P2", "Next Page"))
    ),
    size = "l"))
  })

  # Help "P1"---------------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "P1")
    {showModal(modalDialog(
      title = "CPP Over Time - Page 1 of 2",
      fluidRow(
        tags$img(src = "CPPTimeHelp1.PNG")
      ),
      fluidRow(
        column(10),
        column(2, actionButton("P1P2", "Next Page"))
      ),
      size = "l"))}
  })
  
  observeEvent(input$P1P2,{
    showModal(
      modalDialog(
        title = "CPP Over Time - Page 2 of 2", 
        fluidRow(tags$img(src = "CPPTimeHelp2.PNG")),
        fluidRow(actionButton("P1P1", "Previous Page"))
        , size = "l"))
  })
  
  observeEvent(input$P1P1,{
    showModal(modalDialog(
      title = "CPP Over Time - Page 1 of 2",
      fluidRow(
        tags$img(src = "CPPTimeHelp1.PNG")
      ),
      fluidRow(
        column(10),
        column(2, actionButton("P1P2", "Next Page"))
      ),
      size = "l"))
  })
  
  # Help "P2" ---------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "P2")
    {showModal(modalDialog(
      title = "Compare All CPPs",
      tags$img(src = "AllCPPHelp.PNG"),
      size = "l"))}
  })
  
  # Help "P3" ---------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "P3")
    {showModal(modalDialog(
      title = "Compare Similar CPPs", 
      tags$img(src = "SimCPPHelp.PNG"), 
      size = "l"))}
  })
  
  # Help "InQ" ---------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "InQ")
    {showModal(modalDialog(
      title = "CPP Inequality - Page 1 of 2", 
      tags$img(src = "IneqHelp.PNG"),
      fluidRow(column(10),
               column(2, actionButton("InQ2", "Next Page"))),
      size = "l"))}
  })
  
  observeEvent(input$InQ2,{
    showModal(
      modalDialog(
        title = "CPP Inequality - Page 2 of 2",
      fluidRow(tags$img(src = "IneqHelp2.PNG")),
        fluidRow(column(2, actionButton("InQ1", "Previous Page")),
                 column(10)),
      size = "l"
      )
    )
  })
  
  observeEvent(input$InQ1,
               showModal(modalDialog(
                 title = "CPP Inequality - Page 1 of 2", 
                 tags$img(src = "IneqHelp.PNG"),
                 fluidRow(column(10),
                          column(2, actionButton("InQ2", "Next Page"))),
                 size = "l")))
  
  
  # Help "Vuln" ---------
  observeEvent(input$HelpButton,{
    if(input$tabs == "Vuln"){
      showModal(modalDialog(
        title = "Outcomes for Vulnerable Communties",
        tags$img(src = "VulnCHelp.PNG"),
        size = "l"
      ))
    }
  })
  
  # Help "MyCom" ---------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "MyCom")
    {showModal(
      modalDialog(
        title = "My Communities - Page 1 of 3", 
        fluidRow(tags$img(src = "MyComHelp1.PNG")),
        fluidRow(
          column(10),
          column(2, actionButton("MyComP2", "Next Page"))
        ),
        size = "l"
        )
    )
      }
  })
  
  observeEvent(input$MyComP2,{
      showModal(
        modalDialog(
          title = "My Communities - Page 2 of 3",
          fluidRow(
          tags$img(src = "MyComHelp2.PNG"),
          size = "l"
          ),
          fluidRow(
            column(2, actionButton("MyComP1", "Previous Page")),
            column(8),
            column(2, actionButton("MyComP3", "Next Page"))
          ),
          size = "l"
        )
      )
    })
  
  observeEvent(input$MyComP3,{
    showModal(
      modalDialog(
        title = "My Communities - Page 3 of 3",
        fluidRow(tags$img(src = "MyComHelp3.PNG")),
        fluidRow(actionButton("MyComP2", "Previous Page")),
        size = "l"
      )
    )
  })
  
  observeEvent(input$MyComP1,{
    showModal(
      modalDialog(
        title = "My Communities - Page 1 of 3", 
        fluidRow(tags$img(src = "MyComHelp1.PNG")),
        fluidRow(
          column(10),
          column(2, actionButton("MyComP2", "Next Page"))
        ),
        size = "l"
      )
    )
  })
  
  # Help "CP" ---------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "CP")
    {showModal(
      modalDialog(
        title = "Community Profile - Page 1 of 3",
        fluidRow(tags$img(src = "CPHelp1.PNG")),
        fluidRow(
          column(10),
          column(2, actionButton("CPP2", "Next Page"))
        ),
        size = "l"))}
  })
  
  observeEvent(input$CPP1,{
    showModal(
      modalDialog(
        title = "Community Profile - Page 1 of 3",
        fluidRow(tags$img(src = "CPHelp1.PNG")),
        fluidRow(
          column(10),
          column(2, actionButton("CPP2", "Next Page"))
        ),
        size = "l"))
  })
  
  observeEvent(input$CPP2,{
    showModal(
      modalDialog(
        title = "Community Profile - Page 2 of 3",
        fluidRow(tags$img(src = "CPHelp2.PNG")),
        fluidRow(
          column(2, actionButton("CPP1", "Previous Page")),
          column(8),
          column(2, actionButton("CPP3", "Next Page"))
        ),
        size = "l"))
  })
  
  observeEvent(input$CPP3,{
    showModal(
      modalDialog(
        title = "Community Profile - Page 3 of 3",
        fluidRow(tags$img(src = "CPHelp3.PNG")),
        fluidRow(actionButton("CPP2", "Previous Page")),
        size = "l"))
  })
  
 # Help "allCom" -----------------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "allCom")
    {showModal(modalDialog(title = "All Communities", tags$img(src = "AllCommHelp.PNG"),size = "l"))}
  })
  
  # Help "Map2" -----------------
  
  observeEvent(input$HelpButton,{
    if(input$tabs == "Map2")
    {showModal(modalDialog(title = "Data Zone Comparison",tags$img(src = "DZHelp.PNG"), size = "l"))}
  })
  

  })
