shinyServer(function(input, output, session) {
  
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  
  #Tab 1: Community Map ================================================
  
  #render initial map which shows before CPP selected or clicked
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
      ) %>%
      addLabelOnlyMarkers(lng = -20.6, lat = 60.3,label = HTML("<h2>Community Planning Outcomes Profile</h2><h3>Tracking improvement in communities across Scotland</h3><h5>To get started use the map to select a CPP</h5><h5>Select ‘help with this page’ in the top right hand corner of every page for an introduction to how to use each page</h5><h5>To explore other parts of the CPOP use the list on the left to navigate the tool</h5>"),
                          labelOptions = labelOptions(noHide = T, direction = 'right', offset = c(0,0), textOnly = T, sticky = FALSE))
  })
  
  # Colours for Community Map
  #these are functions
  #FLAG: these seem to generate a maximum of 6 unique colours, but the legend shows 7
  
  clrs      <- brewer.pal(7, "RdYlGn")
  clrsCB    <- rev(brewer.pal(7, "YlGnBu"))
  communityPal <- colorBin(clrs, SpPolysIZ@data$rank_decs)
  communityPalCB <- colorBin(clrsCB, SpPolysIZ@data$rank_decs)
  
  #render map for selected CPP (updated by shape click from scotmap or from CPP drop-down list)
  output$communityMap <- renderLeaflet({
    req(input$LA1)
    sbst <- which(SpPolysIZ@data$council %in% input$LA1)
    dt <- SpPolysIZ[sbst,]
    selCls <- if(input$CBCols){clrsCB}else{clrs}
    selPls <- if(input$CBCols){
      ~communityPalCB(`rank_decs`)
    }
    else{
      ~communityPal(`rank_decs`)}
    topRk <- paste0("Least vulnerable - ",nrow(dt))
    cp <- leaflet(dt) %>%
      addTiles() %>%
      addLegend("bottomright",
                colors = selCls,
                labels = c("Most vulnerable - 1", "","","","","",topRk),
                opacity = 1,
                title = "") %>%
      addPolygons(smoothFactor = 0.5,
                  weight = 1.5,
                  fillOpacity = 0.7,
                  layerId = ~InterZone,
                  fillColor = selPls,
                  color = "black")
  })
  
  #Click map regions to select the CPP (only available on initial load of webpage)
  observe({
    event <- input$scotMap_shape_click
    if(is.null(event)){
      return()} 
    updateSelectizeInput(session,"LA1", label = NULL, choices = NULL, selected = event$id)
  })
  
  # On click make IGZ name popup appear and clear old popups
  observe({
    leafletProxy("communityMap") %>% clearPopups()
    event <- input$communityMap_shape_click
    if(is.null(event)){
      return()}
    isolate({
      showIZPopup(event$id, event$lat, event$lng)
    })
  })
  
  # Function to show IGZ name pop up on shape click
  showIZPopup <- function(group, lat, lng){
    selectedIZ <- SpPolysIZ@data[SpPolysIZ@data$InterZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedIZ$`IGZ name`)))))
    leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Tab 2: CPP Over Time ------------------------------------------------------
  
  #update legend with selected CPP and selected comparison CPP 
  output$CPPLgnd <- renderText({
    txt <- input$LA1
  })
  output$CompLgnd <- renderText({
    txt <- input$CompLA1
  })
  
  #update comparator CPP selection to exclude already selected CPP (from sidebar)
  observeEvent(input$LA1, {
    theseCPPNames <- CPPNames[CPPNames != input$LA1]
    updateSelectInput(session,"CompLA1", label = NULL, choices = c("Scotland",theseCPPNames))
  })
  
  #This reactive statement creates a column which 'tags' the selected local authority with the letter 'A'.
  #and all other LAs with a B which will be used to decide the line colour for the chart output
  tab_2_dta <- reactive({
    CPP_Imp_plus <- add_selected_indicators_tags(CPP_Imp, CPP, input$LA1, input$CompLA1) 
    dta <- filter(CPP_Imp_plus, CPP %in% c(input$LA1, input$CompLA1))
  })

  # loop creating a plot for each indicator
  #continues to line 193
  
  for(i in seq_along(indicators)){
    local({
      current_indicator <- i
      plotname <- paste("plot", current_indicator, sep ="_")
      
      
      output[[plotname]] <- renderPlot({
        req(input$LA1)
        indicatorData <- tab_2_dta() %>%
          filter(Indicator == indicators[current_indicator])
        
        # Y Axis Range for each plot, based on range of full data set
        min_value <- min(indicatorData$value, na.rm = TRUE)
        max_value <- max(indicatorData$value, na.rm = TRUE)
        padding <- (max_value - min_value) * 0.05
        y_min <- min_value - padding
        y_max <- max_value + padding
        
        #EXPLANATION: subsetting data for specific indicator and
        #for the selected CPP and comparator CPP
        indicatorDataCPP1 <- filter(indicatorData, CPP == input$LA1)
        indicatorDataCPP2 <- filter(indicatorData, CPP == input$CompLA1)
        
        # set x axis labels on plots
        # need a column which stores a numeric series to be used as the break points
        # need an additional column which specifies the labels, allowing middle years to be blank
        # the numeric column is also used as a reactive reference point for setting the labels
        #FLAG: these columns could be added to the dataset in global.
        indicatorData <- arrange(indicatorData, CPP)
        indicatorData <- setDT(indicatorData)[, YearBreaks :=(seq(1 : length(Year))), by = CPP] %>%
          mutate(YearLbls = ifelse(YearBreaks == c(1,last(YearBreaks)), as.character(Year), ""))
        indicatorData$YearLbls <- indicatorData$YearLbls %>% str_sub(start = 3)
        year_breaks <- unique(indicatorData$YearBreaks)
        indicatorData$YearLbls[indicatorData$YearBreaks > 1 & indicatorData$YearBreaks < last(indicatorData$YearBreaks)] <- ""
        year_labels <- filter(indicatorData, CPP == input$LA1)$YearLbls
        
        # filter out imputed data for chart lines        
        recorded_data <- indicatorData %>% filter(Type == "Raw data")
        
        # Create plot
        ggplot()+
          geom_line(data = indicatorData,
                    aes(x = YearBreaks,
                        y = value,
                        group = userSelection,
                        colour = userSelection,
                        linetype = "2"),
                    lwd = 1, show.legend = FALSE) +
          geom_line(data = recorded_data,
                    aes(x = YearBreaks, 
                        y = value,
                        group = userSelection,
                        colour = userSelection,
                        linetype = "1"),
                    lwd = 1, 
                    show.legend = FALSE) +
          scale_color_manual(values = c("red", "blue")) +
          labs(title  = indicators[current_indicator]) +
          annotate("text",
                   x = Inf,
                   y = Inf,
                   label = sprintf('\U25CF'),
                   size = 7,
                   colour = trafficLightMarkerColour(indicatorDataCPP1, indicatorDataCPP2),
                   hjust = 1,
                   vjust = 1) +
          scale_x_continuous(breaks = c(1: length(year_breaks)),
                             labels = year_labels) +
          ylim(y_min, y_max) +
          theme(plot.title = element_text(size = 10),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(),
                axis.line = element_line(colour="black"),
                axis.text.x = element_text(vjust = 0.3),
                axis.text.y = element_text(size = 7),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
                )
        })
    })
    }
  
  # Tab 3: Compare All CPPs ------------------------------------------------------------------   
  
  #this is in the server script instead of the UI in order to dynamically generate a drop down
  #of councils with does NOT include the primary CPP (input$LA1)
  #output$CompSelection <- renderUI({
  #  selectizeInput("OtherCPP", "Select a Comparator CPP", choices = CPPNames[CPPNames != input$LA1], 
  #                 options = list(
  #                   placeholder = "Select a CPP",
  #                   onInitialize = I('function() { this.setValue(""); }')))
  #})
  observeEvent(input$LA1, {
    theseCPPNames <- CPPNames[CPPNames != input$LA1]
    updateSelectInput(session,"OtherCPP", choices = theseCPPNames)
  })

  #this is a reactive statement which subsets data by selected CPPs and updates if Selected CPP or Other CPP are altered
  tab_3_dta <- reactive({
  dta <- latest_CPP_Imp %>%
    filter(CPP != "Scotland") %>%
    add_selected_indicators_tags(CPP, input$LA1, input$OtherCPP)
  })
  
#loop to generate a plot for each indicator
  for(i in seq_along(indicators)){
    local({
      current_indicator <- i
      plotnameCPP <- paste("plot_CPP", current_indicator, sep ="_")
      output[[plotnameCPP]] <- renderPlot({
        req(input$LA1)
        if(is.null(input$OtherCPP)){
          sclFll <- scale_fill_manual(values = c("lightblue2","red2"), breaks = c("C", "A"))}
        else{sclFll <- scale_fill_manual(values = c("lightblue2","red2", "green4"), breaks = c("C", "A", "B"))}
        
        indicatorData <- tab_3_dta() %>%
          filter(Indicator == indicators[[current_indicator]])
        
        scotlandValue <- filter(latestScotlandDta, Indicator == indicators[[current_indicator]])$value 
        
        #Generate plots
        ggplot(data = indicatorData) +
          geom_bar(aes(x = if((first(`High is Positive?`))== "Yes"){reorder(CPP, value)}else{reorder(CPP, -value)},
                       y = value,
                       fill = userSelection),
                   stat = "identity",
                   position = "dodge",
                   width = 0.8) +
          sclFll +
          scale_y_continuous(expand = c(0,0), 
                             limits = c(0, max(indicatorData$value)*1.05))+    
          guides(fill = "none") +
          ggtitle(indicators[[current_indicator]]) +
          xlab("") +
          ylab("") +
          geom_hline(aes(yintercept = scotlandValue),
                     colour = "navyblue",
                     size = 1.2) +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                plot.title = element_text(face = "bold", size = 9),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin = unit(c(2,2,15,2),"mm"))
      })
    })
  }
  
  #Tab 4: Compare Similar CPPs ----------------------------------------------------------------
  
  for(i in seq_along(indicators)){
    local({
      current_indicator <- i
      plotnameCPPSim <- paste("plotSimCPP", current_indicator, sep ="_")
      output[[plotnameCPPSim]] <- renderPlot({
        req(input$LA1)
        
        selected_CPP_FG <- filter(CPP_Imp, CPP == input$LA1) %>%
          pull(FG) %>%
          unique()
        
        family_group_dta <- filter(CPP_Imp, Year == RcntYear & FG == selected_CPP_FG) %>%
          add_selected_indicators_tags(CPP, input$LA1) %>%
          filter(Indicator == indicators[[current_indicator]])
        
        scotlandValue <- latestScotlandDta %>%
          filter(Indicator == indicators[[current_indicator]]) %>%
          pull(value)
        
        #Generate plots
        ggplot(data = family_group_dta) +
          geom_bar(aes(x = if(first(`High is Positive?`)== "Yes"){reorder(CPP, value)}
                          else{reorder(CPP, -value)},
                       y = value,
                       fill = userSelection),
                   stat = "identity",
                   position = "dodge",
                   width = 0.5) +
          scale_x_discrete(label = function(x) abbreviate(x, minlength = 10))+
          scale_fill_manual(values = c("lightblue2","red2"), breaks = c("C", "A")) +
          guides(fill = "none") +
          scale_y_continuous(expand = c(0,0), 
                             limits = c(0, max(c(family_group_dta$value, scotlandValue))*1.05))+       
          ggtitle(indicators[[current_indicator]])+
          xlab("")+
          ylab("")+
          geom_hline(aes(yintercept = scotlandValue),
                     colour = "navyblue", size = 1.2)+ 
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust =1, vjust = 0),
                plot.title = element_text(face = "bold", size = 9),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
        })
    })
    }
  
#Tab 5: Inequality over time-------------------------------------------------
  
  #update drop down option for comparator CPP so that it EXCLUDES CPP selected in the sidebar
  observeEvent(input$LA1, {
    theseCPPNames <- CPPNames[CPPNames != input$LA1]
    updateSelectInput(session,"InqComp", label = NULL, choices = c("Scotland",theseCPPNames))
  })
  
  inequalities_datatable <- reactive({
    inequalities_data <- filter(InqDta, CouncilName %in% c(input$LA1, input$InqComp))
    inequalities_data$value <- round(inequalities_data$value, 1)
    inequalities_data <- inequalities_data %>% 
      unite(Titles, Indicator, Year, sep = " - ") %>%
      pivot_wider(names_from = Titles, values_from = value)
    inequalities_data <- inequalities_data[c(2,1,3:10)]
    colnames(inequalities_data)[1:2] <- c("","")
    inequalities_data[2] <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    orderCPPs <- c(input$LA1, input$InqComp)
    inequalities_data <- arrange(inequalities_data, 
                                 match(CouncilName, orderCPPs),
                                 desc(CouncilName))
  })
  
  output$inqTbl <- function(){
    #filter dataset
    req(input$LA1)
    dd <- filter(InqDta, CouncilName %in% c(input$LA1, input$InqComp))
    dd$value <- round(dd$value,1)
    dd <- dd %>% unite(Titles, Indicator, Year, sep = " - ")
    dd <- pivot_wider(dd, names_from = Titles, values_from = value)
    #re-order so that council name comes before decile
    dd <- dd[c(2,1,3:10)]
    dd[2] <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    OrdCPPs <-c(input$LA1, input$InqComp)
    dd <- arrange(dd,match(CouncilName, OrdCPPs), desc(CouncilName))
    #    #mutate each column to add a popver showing the year-DOESN'T WORK IN SHINY
    #   dd <- dd %>% mutate(`Child Poverty (%)` =  cell_spec(`Child Poverty (%)`,popover = spec_popover(content = dd[[1]])))
    colnames(dd)[1:2] <- c("","")
    #data <- inequalities_datatable()
    
    tbl1 <- kable(dd, "html", align = "c", escape = F) %>%
      kable_styling("basic")%>%
      row_spec(0,
               background = "black", 
               color = "white", 
               font_size = 14, 
               align = "right") %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      column_spec(2, bold = TRUE) %>%
      collapse_rows(1, valign = "middle", latex_hline = "major")%>%
      row_spec(3, extra_css = "border-top: solid 1px")  %>%
      row_spec(0, align = "c") %>%
      scroll_box(width = "100%")
    #group_rows("", 3,4, label_row_css = "background-color: black; height: 3px") 
    #group_rows("", 1,2, label_row_css = "height:1px")
    
    # row_spec(tbl1,1, hline_after = TRUE) 
  }
  
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
    ##create colourscheme
    #descText <- "These graphs will help you understand\ninequality in outcomes across the whole of the\nCPP, with 0 indicating perfect equality and\nvalues between 0 and 1 indicating that income\ndeprived people experience poorer outcomes,\n and values between -1 and 0 indicating that\nnon-income deprived people experience\npoorer outcomes."
    DIdta$coloursch <- ifelse(DIdta$la ==input$LA1, "CPP", "Comp")
    lstDi <- lapply(1:7,FUN = function(y){
      dta <- DIdta[DIdta$ind == indList[y],]
      DDta <- filter(dta, year == last(year))
      CDot <- if_else(DDta$Higher == T && DDta$IRHigher == T, "green", if_else(DDta$Higher == F && DDta$IRHigher == F, "red", "yellow"))
      ggplot(dta, aes(x = year, y = value))+
        geom_line(data = dta[!is.na(dta$value),], aes(group = coloursch, colour = coloursch), size = 1)+
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
        scale_x_continuous(breaks = seq(2007,2017, by  =2))+
        geom_hline(yintercept = 0)+
        scale_colour_manual(breaks = c("Comp", "CPP"), values = c("blue", "red"))+
        guides(colour = "none")+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line = element_line(colour="black"))
    })
    #lstDI <- list()
    #lstDI[[1]] <- ggdraw()+draw_text(descText,x = 0.5,y = 0.5, size = 10)
    #lstDI <- c(lstDI,lstDi)
    do.call("plot_grid", c(lstDi, ncol = 4, align = "v"))
  })
  
  #FLAG: Should be in UI script?
 # output$ICompUI <- renderUI({
  #  CPPNames <- CPPNames[CPPNames != input$LA1]
   # selectInput("InqComp", "Select Comparator",
    #            c("Scotland",CPPNames))
  #})
  

  
  #Tab 6: Vulnerable Communities--------------------------------------------
  
  output$HeaderVuln <- renderText({
    req(input$LA1)
    txt <- paste("Have outcomes for the five most vulnerable communities in",input$LA1, "improved faster or slower than the CPP as a whole?")
  })
  
  output$VulnTable <- function(){
    #COMMENT: filters vulnerable communities dataset for selected CPP and removed the CPP column
    CPPSelected <- input$LA1
    vulnCommsData <- VulnComm[,-2] %>% filter(VulnComm$CPP == input$LA1)
      
    #indices of columns which contain change rate observations
    changeRateColumns<- c(6,9,12,15,18,21,24,27)
    #determine text colour based on change rate for each indicator
    vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "Faster"] <- cell_spec(vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "Faster"], color = "green")
    vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "Slower"] <- cell_spec(vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "Slower"], color = "red")
    vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "No Difference"] <- cell_spec(vulnCommsData[,changeRateColumns][vulnCommsData[,changeRateColumns] == "No Difference"], color = "black")
    
    #determine text background colour for overall change rate across all indicators
    vulnCommsData[,3][vulnCommsData[,3] == "Faster"] <- cell_spec(vulnCommsData[,3][vulnCommsData[,3] == "Faster"], color = "white", background = "green")
    vulnCommsData[,3][vulnCommsData[,3] == "Slower"] <- cell_spec(vulnCommsData[,3][vulnCommsData[,3] == "Slower"], color = "white", background = "red")
    vulnCommsData[,3][vulnCommsData[,3] == "No Difference"] <- cell_spec(vulnCommsData[,3][vulnCommsData[,3] == "No Difference"], color = "black", background = "yellow")

    #drop council name column, and bring IZ names column forward to 2nd column
    vulnCommsData <- vulnCommsData[c(2,28,3:27)]
    
    names(vulnCommsData)[1:3] <- c("Vulnerability",
                                 "Name",
                                 "Improvement rate compared to the CPP average")
    
    #whenever it occurs, substitute everything before and including 'Change' with this column name
    names(vulnCommsData) <-gsub(".+Change", "Improvement rate compared to the CPP average", names(vulnCommsData), perl = T)
    #remove everything before and including the last underscore in column names
    names(vulnCommsData) <-gsub("(.+_)", "", names(vulnCommsData), perl = T)
    
    vulnCommsData <- 
      knitr::kable(vulnCommsData,"html", escape = F) %>% 
      kable_styling(bootstrap_options = c("bordered", "hover", "responsive"), 
                    font_size = 14) %>%
      row_spec(0, color = "white", background = "black") %>%
      row_spec(6, background = "aliceblue") %>%
      column_spec(1:2, color = "white", background = "black") %>%
      collapse_rows(columns = 1, valign = "middle") %>%
      add_header_above(c("Community" = 2,
                         "Overall Outcomes" = 1,
                         "Child Poverty" = 3,
                         "Crime Rate*" = 3,
                         "Depopulation" = 3,
                         "Early Mortality" = 3,
                         "Emergency Admissions" = 3,
                         "Out of Work Benefits" = 3,
                         "Positive Destinations" = 3,
                         "Average Highest Attainment" = 3),
                       background = "black",
                       color = "white") %>%
      footnote(symbol = "Crime data is a three year rolled average based on modelled data")%>%
      scroll_box(width = "160%") 
  }  
  
  # Tab 7: My Communities-----------------------------------------------------

  #create rankings for typology and CPP for use later
  IGZBest <- reactive({
    req(input$LA1)
    
    # rankings for outcomes
    if(input$LA1 == "Fife" & input$Fife_SA != "All"){
      IGZBest <- left_join(IGZ_latest, 
                           IGZ_latest_Fife, 
                           by = c("InterZone", "Indicator")) %>% 
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
  })
  
  # Table output 
  
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
  })
  
  
  # Tab 8: Community Profile-------------------------------------- 
  
  output$CommCP <- renderUI({
    IGZsubset <- filter(IGZdta, CPP == input$LA1)
    pickerInput(
      "CommunityCP", 
      "Select a Community", 
      sort(unique(IGZsubset$InterZone_Name)), options = list(size = 8)
    )
  })
  
  output$Descrip <- renderText({
    req(input$LA1)
    IGZsubset <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    txt <- first(IGZsubset$Typology_Name)
    txt <- paste("Group Description: ", txt)
  })
  
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
  
  # create table output
  
  output$CommunityProfileTbl <- DT::renderDataTable({
    req(input$LA1)
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
  })
  
  # Graphs for Community Profile Page
  
  output$LineChoicesCP <- renderUI({
    Choices <- c(input$CommunityCP, input$LA1, "Scotland", "Group Average", "Similar Community")
    awesomeCheckboxGroup(
      "ChoicesCP", 
      "Select lines to plot", status = "danger",
      Choices, selected = c(input$CommunityCP, input$LA1, "Scotland", "Group Average"))
  })
  #outputOptions(output, 'LineChoicesCP', suspendWhenHidden = FALSE)
  output$AddComm <- renderUI({
    Comm <- filter(IGZdta, InterZone_Name == input$CommunityCP & CPP == input$LA1)
    Group <- Comm$Typology_Group[1]
    Options <- filter(IGZdta, Typology_Group == Group)
    Options <- filter(Options, InterZone_Name != input$CommunityCP )
    Options$InterZone_Name <-  paste(Options$CPP, "-",Options$InterZone_Name)
    pickerInput(
      "ChoiceAddComm", 
      "", 
      choices = unique(Options$InterZone_Name), options(size = 10)
    )
  })
  
  observe({
    shinyjs::toggle(id = "AddComm", condition = ("Similar Community" %in% input$ChoicesCP))
  })
  
  
  outputOptions(output, 'AddComm', suspendWhenHidden = FALSE)
  IndicatorsCP <- unique(IGZdta$Indicator)
  
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
    GrpAv              <- filter(GrpAv, InterZone_Name == input$CommunityCP)
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
  })
  
  # Create plot outputs
  
  
  nIndis <- length(IndicatorsCP)
  
  output$CPplots<- renderPlot({
    plots <- list()
    plots <- lapply(1:nIndis, FUN = function(.x){
      req(input$LA1)
      req(input$CommunityCP)
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
  })
  
  #Tab 9: All Communities----------------------------------
  
  output$CommLgnd <- renderText({
    txt <- "Community"
  })
  
  output$CPPLgnd2 <- renderText({
    txt <- input$LA1
  })
  
  output$ScotLgnd <- renderText({
    txt <- "Scotland"
  })
  
  output$CompLgndInq <- renderText({
    txt <- input$InqComp
  })
  output$CPPLgndInq <- renderText({
    txt <- input$LA1
  })
  
  # render plots
  
  myheight <- function(){
    req(input$LA1)
    nrow(unique(IGZdta[IGZdta$CPP == input$LA1,"InterZone_Name"]))*60
  }
  
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
    dta            <- rbind(dta, dta2, dta3)
    
    # Y axis range 
    y_min <- min(dta$value, na.rm = TRUE)
    y_max <- max(dta$value, na.rm = TRUE)
    Rnge <- y_max - y_min
    Extra <- Rnge * 0.05
    y_min <- y_min - Extra
    y_max <- y_max + Extra
    
    dta$colourscheme <-ifelse(
      dta$InterZone_Name == "Scotland",
      "Scot",
      ifelse(
        dta$InterZone_Name == input$LA1,
        "CPP",
        "Com"
      )
    )
    
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
          breaks = c("Com", "CPP", "Scot"), 
          values = c("red", "green4","blue")
        )+
        guides(colour = "none")+
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    })
    do.call("plot_grid", c(plts, ncol = 4))
  }, height = myheight)
  
  
  #colourblind arrows
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
  
  ##Value box for community progress========
  output$comProgressBox <- renderValueBox({
    IGZBest <- IGZBest()
    pBetter <- round((sum(IGZBest$CombinedTypeScore>0)/nrow(IGZBest))*100,0)
    bCol <- if(pBetter <50) {"red"}else{"green"}
    valueBox(
      paste0(pBetter, "%"), "Communities Performing Better than Expected", icon = icon("percent"),
      color = bCol, width = NULL
    )
  })
  ##legend for compar CPP page
  output$BarLA <- renderText({
    txt <- input$LA1
  })
  output$BarComp <- renderText({
    txt <- input$OtherCPP
  })
  output$BarScot <- renderText({
    txt <- "Scotland"
  })
  
  
  #Tab 10. Data Zone Comparison----------------------------
  output$IZUI <- renderUI({
    #FLAG: should be in UI script?
    selectizeInput(
      "IZ", 
      "", 
      choices = sort(unique(CPPMapDta[CPPMapDta$council == input$LA1, 11])),
      options = list(
        placeholder = "Select a Community",
        onInitialize = I('function() { this.setValue(""); }'))
    )
  })
  
  clrs      <- brewer.pal(7, "RdYlGn")
  clrsCB    <- rev(brewer.pal(7, "YlGnBu"))
  povPal    <- colorBin(rev(clrs), SpPolysDF@data$povDecs)
  povPalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$povDecs)
  tariffPal <- colorBin(clrs, SpPolysDF@data$tariffDecs)
  tariffPalCB <- colorBin(clrsCB, SpPolysDF@data$tariffDecs)
  posPal    <- colorBin(clrs, SpPolysDF@data$posDecs)
  posPalCB  <- colorBin(clrsCB, SpPolysDF@data$posDecs)
  benPal    <- colorBin(rev(clrs), SpPolysDF@data$benDecs)
  benPalCB    <- colorBin(rev(clrsCB), SpPolysDF@data$benDecs)
  crimePal  <- colorBin(rev(clrs), SpPolysDF@data$crimeDecs)
  crimePalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$crimeDecs)
  admisPal  <- colorBin(rev(clrs), SpPolysDF@data$admisDecs)
  admisPalCB  <- colorBin(rev(clrsCB), SpPolysDF@data$admisDecs)
  
  plydata <- reactive({
    desIZ <- which(CPPMapDta$council %in% input$LA1 & CPPMapDta$IZname %in% input$IZ)
    IZ_dzs <- SpPolysDF[desIZ,]
  })
  
  # create the map
  #FLAG: variable 'p' is created for every plot for 'newplot' through to
  ## 'newplot6'. Make a function?
  
  output$newplot<-renderLeaflet({
    mapCols <- if(input$CBCols){~povPalCB(`povDecs`)}else{~povPal(`povDecs`)}
    p <- leaflet(plydata()) %>%
      
      # addProviderTiles("OpenStreetMap.HOT")%>% #Humanitarian OpenStreetMap if desired
      
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
    mapCols <- if(input$CBCols){~tariffPalCB(`tariffDecs`)}else{~tariffPal(`tariffDecs`)}
    
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
    mapCols <- if(input$CBCols){~posPalCB(`posDecs`)}else{~posPal(`posDecs`)}
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
    mapCols <- if(input$CBCols){~benPalCB(`benDecs`)}else{~benPal(`benDecs`)}
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
    mapCols <- if(input$CBCols){~crimePalCB(`crimeDecs`)}else{~crimePal(`crimeDecs`)}
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
  
  output$newplot6 <- renderLeaflet({
    mapCols <- if(input$CBCols){~admisPalCB(`admisDecs`)}else{~admisPal(`admisDecs`)}
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
  
  # Clickable popups for map1
  observe({
    leafletProxy("newplot") %>% clearPopups()
    event <- input$newplot_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 13, "newplot")
    })
  })
  
  # Clickable popups for map2
  observe({
    leafletProxy("newplot2") %>% clearPopups()
    event <- input$newplot2_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 14, "newplot2")
    })
  })
  
  # Clickable popups for map3
  observe({
    leafletProxy("newplot3") %>% clearPopups()
    event <- input$newplot3_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 15, "newplot3")
    })
  })
  
  # Clickable popups for map4
  observe({
    leafletProxy("newplot4") %>% clearPopups()
    event <- input$newplot4_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 16, "newplot4")
    })
  })
  
  # Clickable popups for map5
  observe({
    leafletProxy("newplot5") %>% clearPopups()
    event <- input$newplot5_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 17, "newplot5")
    })
  })
  
  # Clickable popups for map6
  observe({
    leafletProxy("newplot6") %>% clearPopups()
    event <- input$newplot6_shape_click
    if (is.null(event))
      return()
    isolate({
      show_DZ_pop_up(event$id, event$lat, event$lng, 18, "newplot6")
    })
  })
  
  #Subset IZ Data
  IZPlys <- reactive({
    
  })
  
  #Tab 11: About/Download-----------------------------------------
  #CPP data
  output$DLDta <- downloadHandler(
    filename = paste("CPP Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/CPP Data - Apr 22.zip", con)
    },
    contentType = "application/zip"
  )
  #
  
  output$DLIZDta <- downloadHandler(
    filename = paste("IGZ Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/IGZ Data - Apr 22.zip", con)
    },
    contentType = "application/zip"
  )
})

#Help Pages==========================

# Work for Help Pages----------------------------------------------

#Community Map Page
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

#CPP Over Time Page
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

#Compare All CPPs page
observeEvent(input$HelpButton,{
  if(input$tabs == "P2")
  {showModal(modalDialog(
    title = "Compare All CPPs",
    tags$img(src = "AllCPPHelp.PNG"),
    size = "l"))}
})

#Compare Similar CPPs page
observeEvent(input$HelpButton,{
  if(input$tabs == "P3")
  {showModal(modalDialog(
    title = "Compare Similar CPPs", 
    tags$img(src = "SimCPPHelp.PNG"), 
    size = "l"))}
})

#CPP Inequality Page

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

##Vulnerable Communities Page
observeEvent(input$HelpButton,{
  if(input$tabs == "Vuln"){
    showModal(modalDialog(
      title = "Outcomes for Vulnerable Communties",
      tags$img(src = "VulnCHelp.PNG"),
      size = "l"
    ))
  }
})

#My Communities Page

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

#Community Profile Page
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

#All Communities Page
observeEvent(input$HelpButton,{
  if(input$tabs == "allCom")
  {showModal(modalDialog(title = "All Communities", tags$img(src = "AllCommHelp.PNG"),size = "l"))}
})

#Data Zone Comparison Page
observeEvent(input$HelpButton,{
  if(input$tabs == "Map2")
  {showModal(modalDialog(title = "Data Zone Comparison",tags$img(src = "DZHelp.PNG"), size = "l"))}
})
