shinyServer(function(input, output, session) {

  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  

  # #shiny alert pop-up------------------------
  # shinyalert(title = "", 
  #            text = tags$div(class="header", 
  #                            checked=NA,
  #                            tags$i(class = "fa-solid fa-circle-info", style="font-size: 42px; color:#295D8A; padding: 15px 15px"),
  #                            tags$p("Welcome", style = "color:#295D8A; font-size: 26px"),
  #                            tags$br(),
  #                            tags$p("The Community Planning Outcomes Profiler is for tracking improvement in communities across Scotland.", style="color:#4682B4"),
  #                            tags$br(),
  #                            HTML("<p style = color:#4682B4><b>To get started </b> use the map (or the dropdown in the sidebar) to select a CPP, then use the menu on the left to explore the data.</p>"),
  #                            ),
  #            html = TRUE)

  
  #dynamic sidebar output-----------------------
  #observe FIRST instance of LA1 input, and render the sidebar menu items in two halves (this is to accommodate the
  #conditional panel (community drop down list) which is rendered between these menu items)
  observeEvent(input$LA1,{
    
    output$firstHalfMenu <- renderMenu({
      req(input$LA1)
      isolate(firstHalfMenuItems <- list(menuItem("CPP Over Time", tabName = "P1", icon = icon("line-chart", verify_fa = FALSE)),
                                         menuItem("Compare All CPPs", tabName = "P2", icon = icon("chart-column", verify_fa = FALSE)),
                                         menuItem("Compare Similar CPPs", tabName = "P3", icon = icon("chart-simple", verify_fa = FALSE)),
                                         menuItem("Inequality Over Time", tabName = "InQ", icon = icon("arrows-v", verify_fa = FALSE)),
                                         menuItem("Vulnerable Communities", tabName = "Vuln",icon = icon("chart-gantt", verify_fa = FALSE)),
                                         menuItem("My Communities", tabName = "MyCom", icon = icon("bars", class = "fa-rotate-90", verify_fa = FALSE)),
                                         menuItem("Community Profile", tabName = "CP", icon = icon("arrow-down", verify_fa = FALSE))
      ))
      isolate(sidebarMenu(firstHalfMenuItems))
    }) %>% #close renderMenu (firstHalfMenu)
      bindEvent(input$LA1, ignoreInit = TRUE, once = TRUE)
    
    output$secondHalfMenu <- renderMenu({
      #req(input$LA1)
      isolate(secondHalfMenuItems <- list( menuItem("All Communities", tabName = "allCom", icon = icon("grip", verify_fa = FALSE)),
                                           menuItem("Data Zone Comparison", tabName = "Map2", icon = icon("globe", verify_fa = FALSE)),
                                           menuItem("About/ Data Download", tabName = "DtaDL", icon = icon("download", verify_fa = FALSE))
      ))
      isolate(sidebarMenu(secondHalfMenuItems))
    }) %>% #close renderMenu (secondHalfMenu)
      bindEvent(input$LA1, ignoreInit = TRUE, once = TRUE)
    
  }, once = TRUE)#close observe event
  
  #dynamically render the select a community drop down depending on whether the CP is currently selected.
  output$communityDropDown <- renderMenu({
    #req(input$LA1)
    conditionalPanel(condition = 'input.tabs == "CP" && input.LA1 !== null', selectInput("CommunityCP", "Select a Community:", choices = communities_list()))
  }) %>% 
    bindEvent(input$LA1, ignoreInit = TRUE, once = TRUE)

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
    sbst <- which(SpPolysIZ$council %in% input$LA1)
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
            x = if((first(`High is Positive`))== "Yes"){reorder(CPP, value)}else{reorder(CPP, -value)}, 
            y = value, 
            fill = colourscheme
          ), 
          stat = "identity",
          position = "dodge",
          #colour = "black",
          width = 0.8
          ) +
          scale_fill_manual(values = c("lightblue2","red2", "green4"), breaks = c("C", "A", "B")) +
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
          theme_bw()+
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
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
            x = if(first(`High is Positive`)== "Yes"){reorder(CPP, value)}else{reorder(CPP, -value)}, 
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
      CDot <- if_else(DDta$Higher == T & DDta$IRHigher == T, "green", if_else(DDta$Higher == F & DDta$IRHigher == F, "red", "yellow"))
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
  
  #"Vuln" vulnerable_change_text ------------
  
  output$vulnerable_change_text <- renderUI({
    req(input$LA1)
    HTML(paste("<br/>This plot shows percentage improvement for the five most vulnerable communities in ",input$LA1, " compared to the CPP average.<br/> 
                 Note that percentage improvement is percentage change towards positive outcomes. For example, a 10% improvement in Child Poverty signifies a 10% <b>reduction</b> since reducing child poverty is the desired outcome. A 10% improvement in Attainment signifies a 10% <b>increase</b> in the average grade attained.
                 <br/>This means that, in this graph, values above 0 indicate change towards positive outcomes for all indicators.<br/><br/>"))
  })
  
  # "Vuln" vulnerable percentage change output ------------
  
  output$vulnerable_change <- renderPlotly({
    
    filtered_change_data <- vulnerable_communities_data %>%
      filter(CPP == input$LA1) %>%
      #select(-value, -CPPAverage) %>%
      arrange(Indicator, vulnerability_rank)
    
    iz_levels <- filtered_change_data %>%
      select(vulnerability_rank, InterZone_Name) %>%
      distinct() %>%
      arrange(vulnerability_rank) %>%
      pull(InterZone_Name)
    
    colours <- c(rev(RColorBrewer::brewer.pal(7, "Blues"))[1:5], "white")
    names(colours) <- iz_levels
    filtered_change_data$InterZone_Name <- factor(filtered_change_data$InterZone_Name, levels = iz_levels, ordered = TRUE)
    filtered_change_data$group <- factor(filtered_change_data$group, levels = filtered_change_data$group, ordered = TRUE)
    
    p <- ggplot(filtered_change_data)+
      suppressWarnings(geom_col(aes(group, ChangeAv, 
                                    fill = CPP,
                                    text = paste0(
                                      "% Improvement since ", BaseYear, ": ", round(ChangeAv,1), "%",
                                      "<br>CPP: ", CPP, 
                                      "<br>Indicator: ", Indicator
                                    )), 
                                color = "indianred2",
                                width = 1)) +
      scale_x_discrete(breaks = filtered_change_data$group[filtered_change_data$vulnerability_rank %in% c(3,6)],
                       labels = str_wrap(filtered_change_data$label[filtered_change_data$vulnerability_rank %in% c(3,6)], 11)
      )+
      geom_segment( aes(x=group, xend=group, y=0, yend=Change, color = InterZone_Name) ) + 
      suppressWarnings(geom_point(aes(x=group, 
                                      y=Change, 
                                      color = InterZone_Name,
                                      text = paste0(
                                        "% Improvement since ", BaseYear, ": ", Change, "%",
                                        "<br> Community: ", InterZone_Name,
                                        "<br> Vulnerability Rank: ", vulnerability_rank,
                                        "<br>Indicator: ", Indicator
                                      )), size=2)) +
      scale_y_continuous(limits = c(max(abs(filtered_change_data$Change))*-1-5, 
                                    max(abs(filtered_change_data$Change))+5),
                         breaks = seq(-80, 80, 10)) + 
      scale_color_manual("Vulnerable Community", values = colours) +
      scale_fill_manual("",values =c("indianred2"),
                        guide = guide_legend(override.aes = list(
                          linetype = "solid",
                          shape = 16))) +
      ylab("% improvement") +
      xlab("Indicator") +
      theme_minimal()
    
    pl <- ggplotly(p,
                   tooltip = "text",
                   height = 620
    ) %>%
      highlight(on = "plotly_hover") %>%
      config(displayModeBar = FALSE) %>%
      layout(
        title = list(text = paste0("% Improvement for Five Most Vulnerable Communities <br> compared to ", filtered_change_data$CPP[1], " Average"),
                     automargin = TRUE),
        margin = list(autoexpand = TRUE,
                      l = 0,
                      r = 0,
                      t = 30,
                      b = 30,
                      pad = 20),
        xaxis = list(autorange = FALSE,
                     fixedrange = TRUE,
                     title = list(standoff = 30),
                     automargin = TRUE
        ),
        yaxis = list(autorange = FALSE,
                     fixedrange = TRUE,
                     title = list(standoff = 30),
                     automargin = TRUE
        ),
        legend = list(itemclick = "toggle")
      )
    
    #ensures that plotly legend items have appropriate value: should be "Community name" not (Community name, 1)
    for (i in 1:length(pl$x$data)){
      if (!is.null(pl$x$data[[i]]$name)){
        pl$x$data[[i]]$name =  gsub("\\(","",str_split(pl$x$data[[i]]$name,",")[[1]][1])
      }
    }
    pl
    
  }) #end of VulnTable 
  
  #"Vuln" vulnerable outcome base year and recent year plot ---------
  
  output$vulnerable_outcomes_plot <- renderPlotly ({
    req(input$LA1)
    
    indicator_data <- vulnerable_communities_data %>%#vul_com_outcomes %>%
      filter(Indicator == input$vuln_indicator,
             CPP == input$LA1) %>%
      arrange(vulnerability_rank)
    
    indicator_data$InterZone_Name <- factor(indicator_data$InterZone_Name, levels = indicator_data$InterZone_Name, ordered = TRUE)
    
    colours <- c(rgb(0.25, 0.57, 0.77, 0.5), 
                 rgb(0.25, 0.57, 0.77, 0.8), 
                 rgb(0.8, 0.3, 0.3, 0.5),
                 rgb(0.8, 0.3, 0.3, 0.8))
    
    names(colours) <-  c(paste0("Community Outcome: ", indicator_data$BaseYear[1]),
                         paste0("Community Outcome: ", RcntYear),
                         paste0("CPP Outcome: ", indicator_data$BaseYear[1]),
                         paste0("CPP Outcome: ", RcntYear))
    
    vuln_by_ind_plot <- ggplot() +
      suppressWarnings(geom_point(data = filter(indicator_data, vulnerability_rank != 6), 
                                  aes(x=InterZone_Name, y=CommunityValue_BaseYear, color=paste0("Community Outcome: ", BaseYear[1]),
                                      text = paste0("Community: ", InterZone_Name,
                                                    "<br>Vulnerability Rank: ", vulnerability_rank,
                                                    "<br>Year: ", BaseYear,
                                                    "<br>Value: ", CommunityValue_BaseYear)),
                                  size=2.5)) +
      suppressWarnings(geom_point(data = filter(indicator_data, vulnerability_rank != 6), 
                                  aes(x=InterZone_Name, y=CommunityValue_RecentYear, color=paste0("Community Outcome: ", RcntYear),
                                      text = paste0("Community: ", InterZone_Name,
                                                    "<br>Vulnerability Rank: ", vulnerability_rank,
                                                    "<br>Year: ", RcntYear,
                                                    "<br>Value: ", CommunityValue_RecentYear)),
                                  size=6)) +
      suppressWarnings(geom_point(data = filter(indicator_data, vulnerability_rank == 6), 
                                  aes(x=CPP, y=CPPAverage_BaseYear, color=paste0("CPP Outcome: ", BaseYear[1]),
                                      text = paste0("CPP: ", InterZone_Name,
                                                    "<br>Year: ", BaseYear,
                                                    "<br>Value: ", CPPAverage_BaseYear)), 
                                  size=2.5 )) +
      suppressWarnings(geom_point(data = filter(indicator_data, vulnerability_rank == 6), 
                                  aes(x=CPP, y=CPPAverage_RecentYear, color=paste0("CPP Outcome: ", RcntYear),
                                      text = paste0("CPP: ", InterZone_Name,
                                                    "<br>Year: ", RcntYear,
                                                    "<br>Value: ", CPPAverage_RecentYear)), 
                                  size=6 )) +
      coord_flip(clip = "off")+
      scale_x_discrete(limits = rev(levels(indicator_data$InterZone_Name))) +
      scale_y_continuous(n.breaks = 8) +
      theme_minimal() +
      theme(
        axis.line = element_line(),
        plot.background = element_blank(),
        panel.border = element_blank()
      ) +
      labs(title = paste0(input$vuln_indicator, 
                          ": Outcomes in ", 
                          indicator_data$BaseYear[1],
                          " and ",
                          RcntYear,
                          "<br> for five most vulnerable communities in ",
                          indicator_data$CPP[1])) +
      xlab("Community") +
      ylab(indicator_data$Indicator[1]) +
      scale_color_manual(name = "",
                         values = colours,
                         guide = guide_legend(override.aes = list(colour = colours,
                                                                  size = c(1,2,1,2) )
                         ))
    
    x_starts <- c(indicator_data$CommunityValue_BaseYear[1:5], indicator_data$CPPAverage_BaseYear[6])
    x_ends <- c(indicator_data$CommunityValue_RecentYear[1:5], indicator_data$CPPAverage_RecentYear[6])
    y_starts <- c(6:1)
    y_ends <- c(6:1)
    
    plot <- ggplotly(vuln_by_ind_plot,
                     tooltip = "text",
                     height = 600) %>%
      layout(xaxis = list(autorange = FALSE,
                          fixedrange = TRUE,
                          title = list(standoff = 30)),
             yaxis = list(autorange = FALSE,
                          fixedrange = TRUE,
                          title = list(standoff = 30)),
             margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
      config(displayModeBar = FALSE)
    
    for (i in 1:6) {
      plot <- plot %>%
        add_annotations(
          x = x_ends[i],
          y = y_ends[i],
          xref = "x",
          yref = "y",
          axref = "x",
          ayref = "y",
          text = "",
          showarrow = TRUE,
          arrowhead = 3,
          arrowsize = 1,
          ax = x_starts[i],
          ay = y_starts[i],
          arrowcolor = "black"
        )
      
    }
    plot
  })
  
  
  IGZBest <- reactive({
    req(input$LA1)
    
    # rankings for outcomes
    if(input$LA1 == "Fife" & input$Fife_SA != "All"){
      IGZBest <- left_join(IGZ_latest, IGZ_latest_Fife, by = c("InterZone", "Indicator")) %>% 
        filter(CPP == "Fife" & `Strategic Area` == input$Fife_SA & Indicator %in% input$IndiMyCom) %>%
        select(-CPPScore)
      IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(SAScore), by = InterZone]
    }else{
      IGZBest <- filter(IGZ_latest, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
      IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(CPPScore), by = InterZone]
    }
    # Filter data so that combined scores are only displayed once for each IGZ
    
    IGZBest <-setDT(IGZBest)[, CombinedTypeScore := sum(TypeScore), by = InterZone]
    IGZBest <- setDT(IGZBest)[, FilterRef:= first(Indicator), by = InterZone]
    IGZBest <- filter(IGZBest, Indicator == FilterRef)
    IGZBest$CPPScoreRank <- rank(IGZBest$CombinedCPPScore)
    IGZBest$TypeScoreRank <- rank(IGZBest$CombinedTypeScore)
    return(IGZBest)
  }) #end of IGZBest()
  
  IGZImprovement <- reactive({
    req(input$LA1)
  if(input$LA1 == "Fife" & input$Fife_SA != "All"){
    IGZImprovement <- left_join(IGZ_change, IGZ_change_Fife, by = c("InterZone", "Indicator")) %>% 
      filter(CPP == "Fife" & 
               `Strategic Area` == input$Fife_SA & 
               Indicator %in% input$IndiMyCom) %>%
      select(-CPPChangeScore)
    IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(SAChangeScore, na.rm= TRUE), by = InterZone]
    IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore, na.rm= TRUE), by = InterZone]
  }else{
    
    IGZImprovement <- filter(IGZ_change, CPP %in% input$LA1 & Indicator %in% input$IndiMyCom)
    IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(CPPChangeScore , na.rm= TRUE), by = InterZone]
    IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore, na.rm= TRUE), by = InterZone]
  }
  
  # Filter data so that combined scores are only displayed once for each IGZ
  IGZImprovement <- setDT(IGZImprovement)[, FilterRef := first(Indicator), by = InterZone]
  IGZImprovement <-filter(IGZImprovement, Indicator == FilterRef)
  IGZImprovement$CPPChangeRank <- rank(IGZImprovement$CombinedCPPChangeScore, ties.method = "min")
  IGZImprovement$TypeChangeRank <- rank(IGZImprovement$CombinedTypeChangeScore, ties.method = "min")
  
  return(IGZImprovement)
  }) ##end of IGZImprovement
  
  #"MyCom" bump_chart_communities_data() ----------
  bump_chart_communities_data <- eventReactive(
    list(input$checkboxupdate, input$Fife_SA),
    {
    req(input$LA1)
    req(IGZBest())
    req(input$IndiMyCom)
    # Split Data into 4 individual DataTables for each ranking, then combine into 1 table
    MyCommunitiesDta <- select(IGZBest(), c(InterZone, InterZone_Name, CPPScoreRank, TypeScoreRank)) %>%
      left_join(select(IGZImprovement(), c(InterZone, InterZone_Name, CPPChangeRank, TypeChangeRank))) %>%
      arrange(CPPScoreRank)
    
    #the following series of steps allocates communities into 1 of 9 colours
    NoIGZ  <- as.numeric(nrow(MyCommunitiesDta))
    Clrs <- ifelse(NoIGZ < 9,NoIGZ,9)
    groupings  <- round(NoIGZ/Clrs)
    Number_seq <- rep(1:Clrs, each = groupings)
    # Check the length of this colour sequence to determine 
    # whether more needs to be added or some need to be remove
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
    unq <- unique(Complete_seq)
    ngroups <- length(unq)
    #create ordered factor character column for colour group for named discrete color scale later
    MyCommunitiesDta$colourIndex <- factor(as.character(Complete_seq), levels = unq, ordered = TRUE)
    
    names(MyCommunitiesDta) <- c("IZ", "IZName", "CPP Outcomes", "Similar Communities Outcomes", "CPP Improvement", "Similar Communities Improvement", "colourIndex" )
    
    #pivot longer with metric to Category and rank to Rank, 
    # also creates arbitrary year column (which will be used as basis for continous time axis for bump chart before labels replaced)
    my_comm_long <- MyCommunitiesDta %>%
      pivot_longer(cols = c("CPP Outcomes", "Similar Communities Outcomes", "CPP Improvement", "Similar Communities Improvement"),
                   names_to = "Category",
                   values_to = "Rank") %>%
      mutate(Year = case_when(Category == "CPP Outcomes" ~ 2019,
                              Category == "Similar Communities Outcomes" ~ 2020,
                              Category == "CPP Improvement" ~ 2021,
                              Category == "Similar Communities Improvement" ~ 2022))
    
    return(my_comm_long)
  },
  ignoreNULL = FALSE)
  
  # "MyCom" my_communities_header / my_communities_intro output --------
  
  # output$my_communities_header <- renderUI({
  #   req(input$LA1)
  #   indicator_text <- if_else(input$IndiMyCom == "All", " across all indicators", paste0(" for ", input$IndiMyCom))
  #   HTML(paste0("<b>Which communities have the poorest outcomes and improvement rate in ", input$LA1,
  #               indicator_text, "?</b>"))
  # })
  # 
  # 
  # output$my_communities_intro <- renderUI({
  #   req(input$LA1)
  #   
  #   indicator_text <- if_else(input$IndiMyCom == "All", " across all indicators", paste0(" in ", input$IndiMyCom))
  #   HTML(paste0("Here all communities in ", input$LA1," are ordered by their performance ",indicator_text, " by four metrics:<br/>
  #   <ul><li>outcome (poorest to best outcome)</li>
  #   <li>improvement since base year (least to most improved)</li>
  #   <li>outcome compared to similar communities across Scotland (communities ordered by distance below their respective similar community average)</li>
  #   <li>improvement compared to similar communities across Scotland (communities ordered by distance below their respective similar community average)</li></ul>"))
  # })
  
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
  
  #"My Com" community_bump_chart output-------------------
  output$community_bump_chart <- renderPlotly({
    req(input$LA1)
    
    my_communities_data <- bump_chart_communities_data()
    n_communities <- length(unique(my_communities_data$IZ))
    
    #filter data by first/last 5/10 if selected
    show <- input$View
    top10communities <- my_communities_data$IZ[my_communities_data$Category == "CPP Outcomes" & my_communities_data$Rank <= 10]
    bottom10communities <- my_communities_data$IZ[my_communities_data$Category == "CPP Outcomes" & my_communities_data$Rank >= n_communities - 10]
    
    
    if(show == "Most/Least 5") {
      my_communities_data <- my_communities_data %>%
        filter(IZ %in% top10communities[1:5] | IZ %in% bottom10communities[6:10]) %>%
        unique()
      
      
    } else if (show == "Most/Least 10") {
      my_communities_data <- my_communities_data %>%
        filter(IZ %in% top10communities | IZ %in% bottom10communities) %>%
        unique()
      
    }
    
    else if (show == "All") {
      my_communities_data <- bump_chart_communities_data()
      
    }
    
    #define colour list
    n_colour_groups <- length(unique(my_communities_data$colourIndex))
    colour_groups <- unique(as.numeric(my_communities_data$colourIndex))
    max_rank <- max(my_communities_data$Rank)
    
    
    if(show == "All") {
      if(input$CBCols){my_comm_colours <- c(rev(RColorBrewer::brewer.pal("YlGnBu", n = 9))[1:9])}
      else{my_comm_colours <- c(RColorBrewer::brewer.pal("RdYlGn", n = 9)[1:9])}
    } 
    else if (show == "Most/Least 10" | show == "Most/Least 5") {
      if(input$CBCols){my_comm_colours <- c(rev(RColorBrewer::brewer.pal("YlGnBu", n = 9))[colour_groups])}
      else{my_comm_colours <- c(RColorBrewer::brewer.pal("RdYlGn", n = 9)[colour_groups])}
    }
    
    
    #assign colorIndex character levels to colours so that colours are consistent even when top5/top10 selected
    names(my_comm_colours) <- unique(my_communities_data$colourIndex)# label_levels  
    
    # labels for colScale functions for n_colour_groups, minimum two groups (i.e Glasgow City)
    col_labels <- c(paste0("Poorest Outcome in ", RcntYear), rep(" ", n_colour_groups - 2), paste0("Best Outcome in ", RcntYear))
    
    
    #fill colour scale for geom_points
    colScalePoint <- scale_fill_manual(name = " ",#name = "Click legend to view one colour group",
                                       values = my_comm_colours, 
                                       labels = col_labels,
                                       guide = guide_legend(
                                         override.aes = list(
                                           linetype = rep("solid", n_colour_groups),
                                           shape = rep(21, n_colour_groups)
                                         )))
    
    #line colour scale for geom_bump
    colScaleLine <- scale_color_manual(name = " ", #name = "Hover to highlight one community,<br>double-click white space to deselect ",
                                       values = my_comm_colours, 
                                       labels = col_labels,
                                       guide = guide_legend(
                                         override.aes = list(
                                           linetype = rep("solid", n_colour_groups),
                                           shape = rep(21, n_colour_groups)
                                         )))
    
    
    #"My Comm" bump chart ggplot ---------------
    
    #shared data object - this allows for click highlighting for all plotly traces which use group = IZ aesthetic
    shared_communities_data <- highlight_key(my_communities_data, ~IZ)
    
    
    p <- ggplot(shared_communities_data, aes(x = Year, 
                                             y = Rank, 
                                             group = IZ)) +
      suppressWarnings(geom_bump(size = 1.5, alpha = 0.7, aes(colour = colourIndex, text = IZName))) +
      suppressWarnings(geom_point(size = 4, colour="black",pch=21,
                                  aes(fill = colourIndex,
                                      text = paste0("Community: ", IZName,
                                                    "<br>Code: ", IZ,
                                                    "<br>Position: ", Rank, "/", n_communities,
                                                    "<br>Measure: ", Category)))) + 
      scale_y_reverse(name = "Position",
                      limits = c(max_rank,0),
                      breaks = seq(0, n_communities,5)) + 
      scale_x_continuous(name = "",
                         limits = c(2017.7, 2022.2),
                         breaks = seq(2018, 2022, 1),
                         labels = c(" ", "<b>Within CPP which \ncommunities have \nthe poorest \noutcomes?</b>",
                                    "<b>Compared to \nsimilar communities \noutwith the CPP, \ndo communities \nfare better \nor worse than expected?</b>",
                                    "<b>Within CPP which \ncommunities improved \nthe least?</b>",
                                    "<b>Within CPP which \ncommunities improved \n the least relative to \nsimilar communities?</b>"),
      ) + 
      colScalePoint +
      colScaleLine +
      geom_text(data = filter(my_communities_data, Year == min(Year)),
                aes(x = 2018.8, label = IZName), 
                size = 2.5, 
                hjust = 1, 
                vjust = 0.5) +
      coord_cartesian(clip = "off") + 
      theme_minimal() + 
      theme(panel.grid.minor = element_blank(),
            legend.position =  "none")
    
    
    #"My Comm" bump chart ggplotly conversion ------------    
    plot <- ggplotly(p,
                     tooltip = "text",
                     height = max(700,n_communities*18)
    ) %>% #ensure height dynamically changes with number of IGZs in CPP
      highlight(on = "plotly_click", 
                off = "plotly_doubleclick",
                selected = attrs_selected(showlegend = FALSE),
                opacityDim = 0.15) %>%
      style(textposition = "left") %>%
      config(displayModeBar = FALSE) %>%
      layout(autosize = TRUE,
             xaxis = list(autorange = FALSE,
                          fixedrange = TRUE,
                          side = "top"
             ),
             yaxis = list(autorange = FALSE,
                          fixedrange = TRUE),
             legend = list(itemclick = "toggleothers")
      )
    
    #replace colourIndex legend labels ("1": "9") with "Poorest Outcome/Best Outcome and blanks
    for (i in 1:length(plot$x$data)){
      if (!is.null(plot$x$data[[i]]$name)){
        if(plot$x$data[[i]]$name == "1"){
          plot$x$data[[i]]$name <- paste0("Poorest Outcome<br>in ", RcntYear)
        } else if (gsub("\\(","",str_split(plot$x$data[[i]]$name,",")[[1]][1]) == rev(levels(my_communities_data$colourIndex))[1]) {
          plot$x$data[[i]]$name <- paste0("Best Outcome<br>in ", RcntYear)
        }else {
          plot$x$data[[i]]$name <- " "
        }
      }
      if (!is.null(plot$x$data[[i]]$mode)){
        if(plot$x$data[[i]]$mode == "markers"){
          plot$x$data[[i]]$showlegend <- TRUE
        } else if (plot$x$data[[i]]$mode == "lines") {
          plot$x$data[[i]]$showlegend <- FALSE
        }
      }
    }
    
    plot
    
  })#end of community bump chart
  
  ##Value box for community progress========
  output$comProgressBox <- renderValueBox({
    IGZBest <- IGZBest()
    pBetter <- round((sum(IGZBest$CombinedTypeScore>0)/nrow(IGZBest))*100,0)
    bCol <- if(pBetter <50) {"red"}else{"green"}
    valueBox(
      paste0(pBetter, "%"), "Communities Performing Better than Expected", icon = icon("percent"),
      color = bCol, width = NULL
    )
  }) %>% bindEvent(
    list(input$checkboxupdate, input$Fife_SA))
  
  
  
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
    req(input$LA1, input$CommunityCP)
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
    req(input$LA1)
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
    req(input$LA1)
    
    selectizeInput(
      "IZ", 
      "Select a Community:", 
      choices = sort(unique(pull(IGZdta[IGZdta$CPP == input$LA1, "InterZone_Name"]))),
      selected = sort(unique(pull(IGZdta[IGZdta$CPP == input$LA1, "InterZone_Name"])))[1]
    )
  })
  
  # "Map2" plydata() for leaflet outputs --------------
  plydata <- reactive({
    req(input$LA1)
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
      showDZpopup(CPPMapDta, event$id, event$lat, event$lng, "Crimes per", "newplot4")
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
      file.copy("data/CPP Data - Oct 23.zip", con)
    },
    contentType = "application/zip"
  )
  
  #IZ data
  output$DLIZDta <- downloadHandler(
    filename = paste("IGZ Data", ".zip", sep = ""),
    content = function(con) {
      file.copy("data/IGZ Data - Sep 23.zip", con)
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
