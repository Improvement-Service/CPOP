shinyServer(function(input, output, session) {
 
  # Create Ui ouputs for CPP over time page - PAGE1------------------------------------------------------
  output$CPPLgnd <- renderText({
    txt <- input$LA1
  })
  
  output$CompLgnd <- renderText({
    txt <- input$CompLA1
  })
  
  
  
  selected_dta_1 <- reactive({
    CPP_Imp$colourscheme <- ifelse(CPP_Imp$CPP == input$LA1,"A","B")
    dta <- filter(CPP_Imp, CPP %in% c(input$LA1, input$CompLA1))
  })
  
  # List indicators in the order these are to be presented
  
  indicators_1 <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
                    "Attainment", "Positive Destinations", "Employment Rate",
                    "Median Earnings", "Out of Work Benefits", "Business Survival",
                    "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
                    "Emergency Admissions", "Unplanned Hospital Attendances",
                    "Early Mortality", "Fragility", "Well-being", "Fuel Poverty"
  )
  
  # loop creating a plot for each indicator   
  
  for(i in seq_along(indicators_1)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        req(input$LA1)
        dta <- selected_dta_1()
        
        # Y Axis Range for each plot, based on range of full data set
        
        y_rnge_dta <- subset(CPP_Imp, CPP_Imp$Indicator == indicators_1[my.i])
        y_min <- min(y_rnge_dta$value, na.rm = TRUE)
        y_max <- max(y_rnge_dta$value, na.rm = TRUE)
        Rnge <- y_max - y_min
        Extra <- Rnge * 0.05
        y_min <- y_min - Extra
        y_max <- y_max + Extra
        
        loopdata <- subset(dta, dta$Indicator == indicators_1[my.i])
        loopdata_CPP1 <- filter(loopdata, CPP == input$LA1)
        loopdata_CPP2 <- filter(loopdata, CPP == input$CompLA1)
        
        # If statement to determine the colour of the dot
        # 2 statements to distinguish between high values being positive and high values being negative
        # compares two selected authorities by the latest value and the improvement rate 
        
        coloursDotPos <- if_else(
          ((last(loopdata_CPP1$value)) > (last(loopdata_CPP2$value)) & 
             (last(loopdata_CPP1$Improvement_Rate)) > (last(loopdata_CPP2$Improvement_Rate))),
          "green",
          if_else(
            ((last(loopdata_CPP1$value)) > (last(loopdata_CPP2$value)) & 
               (last(loopdata_CPP1$Improvement_Rate)) < (last(loopdata_CPP2$Improvement_Rate))),
            "yellow",
            if_else(
              ((last(loopdata_CPP1$value)) < (last(loopdata_CPP2$value)) &
                 (last(loopdata_CPP1$Improvement_Rate)) > (last(loopdata_CPP2$Improvement_Rate))),
              "yellow",
              if_else(
                ((last(loopdata_CPP1$value)) < (last(loopdata_CPP2$value)) & 
                   (last(loopdata_CPP1$Improvement_Rate)) < (last(loopdata_CPP2$Improvement_Rate))),
                "red",
                "black"
              )
            )
          )
        )
        
        coloursDotNeg <- if_else(
          ((last(loopdata_CPP1$value)) > (last(loopdata_CPP2$value)) &
             (last(loopdata_CPP1$Improvement_Rate)) > (last(loopdata_CPP2$Improvement_Rate))),
          "red",
          if_else(
            ((last(loopdata_CPP1$value)) > (last(loopdata_CPP2$value)) &
               (last(loopdata_CPP1$Improvement_Rate)) < (last(loopdata_CPP2$Improvement_Rate))),
            "yellow",
            if_else(
              ((last(loopdata_CPP1$value)) < (last(loopdata_CPP2$value)) &
                 (last(loopdata_CPP1$Improvement_Rate)) > (last(loopdata_CPP2$Improvement_Rate))),
              "yellow",
              if_else(
                ((last(loopdata_CPP1$value)) < (last(loopdata_CPP2$value)) &
                   (last(loopdata_CPP1$Improvement_Rate)) < (last(loopdata_CPP2$Improvement_Rate))),
                "green",
                "black"
              )
            )
          )
        )
        
        # store unique values of "high is positive?" to determine which coloured dot to use
        
        HighValue <- unique(loopdata$`High is Positive?`)
        
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
          geom_line(
            data = dtaRaw,
            aes(
              x = YearBreaks, 
              y = value, 
              group = colourscheme, 
              colour = colourscheme, 
              linetype = "1"
            ), 
            lwd = 1, show.legend = FALSE
          )+
          scale_color_manual(values = c("red", "blue"))+
          labs(title  = indicators_1[my.i])+
          annotate(
            "text", 
            x = Inf, 
            y = Inf, 
            label = sprintf('\U25CF'), 
            size = 7, 
            colour = (if_else(HighValue == "Yes", coloursDotPos, coloursDotNeg)), 
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
          )
        
      })
    })  
  }
  
  # Create Ui Outputs for CPP all 32 - PAGE2 ------------------------------------------------------------------   
  output$CompSelection <- renderUI({
    selectizeInput("OtherCPP", "Select a Comparator CPP", choices =CPPNames[CPPNames!= input$LA1], 
            options = list(
             placeholder = "Select a CPP",
             onInitialize = I('function() { this.setValue(""); }')))
    })
  
#Render all Plots for Page 2===================================
  
  # List indicators in the order these are to be presented
  
  indis <- c("Healthy Birthweight", "Primary 1 Body Mass Index", "Child Poverty",
             "Attainment", "Positive Destinations", "Employment Rate",
             "Median Earnings", "Out of Work Benefits", "Business Survival",
             "Crime Rate", "Dwelling Fires", "Carbon Emissions", 
             "Emergency Admissions", "Unplanned Hospital Attendances",
             "Early Mortality", "Fragility", "Well-being", "Fuel Poverty")
  
  for(i in seq_along(indis)){
    local({
      this_i <- i
      plotnameCPP <- paste("plot_CPP", this_i, sep ="_")
      output[[plotnameCPP]] <- renderPlot({
    req(input$LA1)
    dta <- filter(CPP_Imp, Year == RcntYear)
    if(is.null(input$OtherCPP)){
      dta$colourscheme <- ifelse(dta$CPP == input$LA1,"Sel1","Other")}
    else{
      dta$colourscheme <- ifelse(dta$CPP == input$LA1,"Sel1",ifelse(dta$CPP == input$OtherCPP, "Sel2","Other"))
      }
    if(is.null(input$OtherCPP)){
      sclFll <- scale_fill_manual(values = c("lightblue2","red2"), breaks = c("Other", "Sel1"))}
    else{sclFll <- scale_fill_manual(values = c("lightblue2","red2", "green4"), breaks = c("Other", "Sel1", "Sel2"))}
    #filter so that the Scotland value isn't a bar on the plot
    
    dtaNoScot <- filter(dta, CPP != "Scotland")
  
    #Generate plots
    indi <- indis
    ##calculate maximum limit for y axis  
      maxAx <- max(dtaNoScot[dtaNoScot$Indicator == indi[[this_i]],5])*1.05
      minAx <- ifelse(indi[[this_i]] == "Carbon Emissions",-5,0)
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
        sclFll+
        #scale_x_discrete(label = function(x) abbreviate(x, minlength = 4))+
        scale_y_continuous(expand = c(0,0), limits = c(minAx, maxAx))+    
        guides(fill = FALSE) +
        ggtitle(indi[[this_i]])+
        xlab("")+
        ylab("")+
        #    {if(input$ScotCheckbox == TRUE)
        geom_hline(aes(
          yintercept = filter(dta, CPP == "Scotland" & Indicator == indi[[this_i]])$value
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
  }
  
  # Create Graphs for CPP similar - PAGE3----------------------------------------------------------------
  for(i in seq_along(indis)){
    local({
      that_i <- i
      plotnameCPPSim <- paste("plotSimCPP", that_i, sep ="_")
      output[[plotnameCPPSim]] <- renderPlot({
        req(input$LA1)
        FGroup <- filter(CPP_Imp, CPP == input$LA1)[[1,7]]
        dta <- filter(CPP_Imp, Year == RcntYear & FG %in% FGroup)
        dta$colourscheme <-ifelse(dta$CPP == input$LA1,"Sel1","Other")
        #filter so that the Scotland value isn't a bar on the plot
        
        dtaNoScot <- filter(dta, CPP != "Scotland")
        
        #Generate plots
        indi <- indis
        ##calculate maximum limit for y axis  
        ScotVal <- filter(CPP_Imp,Year == RcntYear & 
                            Indicator == indi[[that_i]] &
                            CPP == "Scotland")$value
        maxAx <- max(c(dta[dta$Indicator == indi[[that_i]],]$value, ScotVal))*1.05
        minAx <- ifelse(indi[[that_i]] == "Carbon Emissions" & FGroup %in% c(2,3),-5,0)
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
          scale_fill_manual(values = c("lightblue2","red2"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
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
  }

  # Create Ui outputs for Maps - PAGE4&5--------------------------------------------------

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
  
  output$newplot<-renderLeaflet({
    mapCols <- if(input$CBCols){~povPalCB(`povDecs`)}else{~povPal(`povDecs`)}
    p <- leaflet(plydata())%>%
      
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
  
  showDZPopup <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s",
        "Children in Poverty (%)", 
        round(unique(as.numeric(selectedDZ[13])),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot") %>% clearPopups()
    event <- input$newplot_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup(event$id, event$lat, event$lng)
    })
  })
  
  # Clickable popups for map2
  
  showDZPopup2 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s\n",
        "Average Highest Attainment", 
        round(unique(selectedDZ[14]),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot2") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot2") %>% clearPopups()
    event <- input$newplot2_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup2(event$id, event$lat, event$lng)
    })
  })
  
  # Clickable popups for map3
  
  showDZPopup3 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s\n",
        "Positive Destinations (%)", 
        round(unique(selectedDZ[15]),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot3") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot3") %>% clearPopups()
    event <- input$newplot3_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup3(event$id, event$lat, event$lng)
    })
  })
  
  # Clickable popups for map4
  
  showDZPopup4 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s\n",
        "Out of Work Benefits (%)", 
        round(unique(selectedDZ[16]),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot4") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot4") %>% clearPopups()
    event <- input$newplot4_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup4(event$id, event$lat, event$lng)
    })
  })
  
  # Clickable popups for map5
  
  showDZPopup5 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s\n",
        "SIMD Crimes per 10,000", 
        round(unique(selectedDZ[17]),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot5") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot5") %>% clearPopups()
    event <- input$newplot5_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup5(event$id, event$lat, event$lng)
    })
  })
  
  # Clickable popups for map6
  
  showDZPopup6 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf(
        "%s: %s\n",
        "Emergency Admissions per 100,000", 
        round(unique(selectedDZ[18]),2)
      ), 
      tags$br()
    ))
    leafletProxy("newplot6") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  # Makes the popups appear and clears old popups
  
  observe({
    leafletProxy("newplot6") %>% clearPopups()
    event <- input$newplot6_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup6(event$id, event$lat, event$lng)
    })
  })
  
  # Colours for Community Map
  
  communityPal <- colorBin(clrs, SpPolysIZ@data$rank_decs)
  communityPalCB <- colorBin(clrsCB, SpPolysIZ@data$rank_decs)
  
  #Subset IZ Data
  IZPlys <- reactive({
    
  })
  
  # Create Community Map
  
  output$communityMap <- renderLeaflet({
    req(input$LA1)
    sbst <- which(SpPolysIZ@data$council %in% input$LA1)
    dt <- SpPolysIZ[sbst,]
    selCls <- if(input$CBCols){clrsCB}else{clrs}
    selPls <- if(input$CBCols){
      ~communityPalCB(`rank_decs`)
    }else{~communityPal(`rank_decs`)}
    topRk <- paste0("Least vulnerable - ",nrow(dt))
    cp <- leaflet(dt) %>%
      addTiles() %>%
      addLegend("bottomright", colors = selCls,
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
  })
  
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
  
  ##Click to select the CPP
  observe({
    event <- input$scotMap_shape_click
    if(is.null(event)){
      return()} 
      updateSelectizeInput(session,"LA1", label = NULL, choices = NULL, selected = event$id)
  })
  
  # Add click function
  
  showIZPopup <- function(group, lat, lng){
    selectedIZ <- SpPolysIZ@data[SpPolysIZ@data$InterZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedIZ$`IGZ name`)))#,
      #paste("Community Ranking:", as.character(unique(selectedIZ[14]))),
     # tags$br()
    ))
    leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
  }
  
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
  
  #Create Ui outputs for Vulnerable Communities page - PAGE 6 --------------------------------------------
  
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
  
    
    names(CompleteSet) <- c(
      " ",
      "Community",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average",
      "2006/07",
      "2017/18",
      "Improvement rate compared to the CPP average"
    )
    
    CompleteSet <- 
      knitr::kable(CompleteSet,"html", escape = F) %>% 
      kable_styling(bootstrap_options = c("bordered", "hover", "responsive"), 
                    font_size = 14) %>%
      add_header_above(c(
        " " = 2,
        "Overall Outcomes" = 1,
        "Child Poverty" = 3,
        "Crime Rate" = 3,
        "Depopulation" = 3,
        "Early Mortality" = 3,
        "Emergency Admissions" = 3,
        "Out of Work Benefits" = 3,
        "Positive Destinations" = 3,
        "Average Highest Attainment" = 3
      )
      ) %>%
      collapse_rows(columns = 1, valign = "middle") %>%
      scroll_box(width = "160%") 
  }  
  
  # Create Ui ouputs for My Communities Page - PAGE 7-----------------------------------------------------
  
  
#  observeEvent(eventExpr = input$IndiAll,
 #              handlerExpr = {
  #               updateCheckboxGroupInput(session = session,
  #                                        inputId = "IndiMyCom",
  #                                        selected = unique(IGZdta$Indicator)
  #               )
  #             }
  #)
  
  #observe({
  #  if(input$IndiClear >0){
  #    updateCheckboxGroupInput(session = session, 
  #                             inputId = "IndiMyCom",
  #                             selected = character(0)
  #    )
  #  }
  #})  
  

 ##create rankings for typology and CPP for use later
  IGZBest <- reactive({
    req(input$LA1)
    
    # rankings for outcomes
    
    IGZBest <- filter(IGZ_latest, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
    IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(CPPScore), by = InterZone]
    IGZBest <-setDT(IGZBest)[, CombinedTypeScore := sum(TypeScore), by = InterZone]
    
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
    
    IGZImprovement <- filter(IGZ_change, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
    IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(CPPChangeScore), by = InterZone]
    IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    
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
    
    MyCommunitiesDta[,ncol(MyCommunitiesDta) + 1] <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,9,2,3,4,5,6,7,8)]
    
    MyCommunitiesDta[,ncol(MyCommunitiesDta) +1] <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,2,3,10,4,5,6,7,8,9)]
    
    MyCommunitiesDta[,ncol(MyCommunitiesDta) + 1] <- NA
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
    Top10[nrow(Top10) + 2,] <- NA
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
    Top5[nrow(Top5)+2,] <- NA
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
  
  
  # Create Ui ouputs for Community Profile - Page 8-------------------------------------- 
  
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
    
    CommunityProfileDta[,ncol(CommunityProfileDta)+1] <- NA
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
    Top10[nrow(Top10) +2,] <- NA
    Bottom10 <- tail(CommunityProfileDta, Bottom10Rows)
    TopBottom10 <- rbind(Top10, Bottom10)
    
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
    Top5[nrow(Top5)+2,] <- NA
    Bottom5 <- tail(CommunityProfileDta, Bottom5Rows)
    TopBottom5 <- rbind(Top5, Bottom5)
    
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
  
  # Create Ui Outputs for All Communities Page - PAGE 9----------------------------------
  
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
    # Y axis range 
    y_rnge_dta <- filter(
      IGZdta, IGZdta$CPP==input$LA1 &
      IGZdta$IndicatorFullName == input$IndiAllC & IGZdta$Type != "Projected"
    )
    y_min <- min(y_rnge_dta$value, na.rm = TRUE)
    y_max <- max(y_rnge_dta$value, na.rm = TRUE)
    Rnge <- y_max - y_min
    Extra <- Rnge * 0.05
    y_min <- y_min - Extra
    y_max <- y_max + Extra
    
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
        guides(colour = FALSE)+
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
    pBetter <- round((sum(IGZBest$TypeScore>0)/nrow(IGZBest))*100,0)
    bCol <- if(pBetter <50) {"red"}else{"green"}
    valueBox(
    paste0(pBetter, "%"), "Communities Performing Better than Expected", icon = icon("percent"),
    color = bCol, width = NULL
    )
  })
  
  # Create Ui Outputs for Inequality Page -------------------------------------------------
  output$inqTbl <- function(){
    #filter dataset
    req(input$LA1)
    dd <- filter(InqDta, CouncilName %in% c(input$LA1, input$InqComp) & Year == input$InqYr)
    dd$value <- round(dd$value,1)
    dd <- spread(dd, Indicator, value)[2:11]
    ##Rearrange indicators - if adding OOWB make: dd[c(2,1,3:10)]
    dd <- dd[c(2,1,3:7,9:10)]
    dd[2] <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    OrdCPPs <-c(input$LA1, input$InqComp)
    dd <- arrange(dd,match(CouncilName, OrdCPPs), desc(CouncilName))
    #rownames(dd) <- c("Least deprived","Least deprived","Most deprived", "Most deprived")
    colnames(dd)[1:2] <- c("","")
    tbl1 <- kable(dd, "html")%>% 
      kable_styling("basic")%>%
      row_spec(0,background = "black", color = "white", font_size = 14, 
               align = "right") %>%
      column_spec(1, bold = TRUE, border_right = TRUE) %>%
      column_spec(2, bold = TRUE) %>%
      collapse_rows(1,valign = "middle",latex_hline = "major")%>%
      row_spec(3, extra_css = "border-top: solid 1px") 
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
      DDta <- filter(dta, year == 2017)
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
        scale_y_continuous(limits = c(-0.23,0.5))+
        scale_x_continuous(breaks = seq(2007,2017, by  =2))+
        geom_hline(yintercept = 0)+
        scale_colour_manual(breaks = c("Comp", "CPP"), values = c("blue", "red"))+
        guides(colour = FALSE)
    })
    #lstDI <- list()
    #lstDI[[1]] <- ggdraw()+draw_text(descText,x = 0.5,y = 0.5, size = 10)
    #lstDI <- c(lstDI,lstDi)
    do.call("plot_grid", c(lstDi, ncol = 4, align = "v"))
  })
  

  
  output$ICompUI <- renderUI({
    CPPNames <- CPPNames[CPPNames != input$LA1]
    selectInput("InqComp", "Select Comparator",
                c("Scotland",CPPNames))
  })


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
      title = "CPP Inequality", 
      tags$img(src = "IneqHelp.PNG"),
      size = "l"))}
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
    
  ##Download data buttons============
    #CPP data
    output$DLDta <- downloadHandler(
    filename = paste("CPP Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/CPP Data - Feb 19.zip", con)
    },
    contentType = "application/zip"
    )
    #
    
    output$DLIZDta <- downloadHandler(
      filename = paste("IGZ Data", ".zip", sep = ""),
      content = function(con) {
        file.copy("data/IGZ Data - Feb 19.zip", con)
      },
      contentType = "application/zip"
    )
  })
