shinyServer(function(input, output,session) {
  
  
  
  ##Create Ui ouputs for page 1=============
  
  #Create a reactive function to store data for both LA's selected  
  selectedDta1 <- reactive({
    CPPdtaCurrent$colourscheme <- ifelse(CPPdtaCurrent$CPP == input$LA1,"A","B")
    dta <- filter(CPPdtaCurrent, CPP %in% c(input$LA1, input$CompLA1))
  })
  
  #Create a list of all the indicators 
  Indicators1 <- unique(CPPdtaCurrent$Indicator)
  
  
  ##########
  #Create a loop that creates a plot for the indicators selected 
  
  for(i in seq_along(Indicators1)){
    local({
      my.i <- i
      plotname <- paste("plot", my.i, sep ="_")
      output[[plotname]] <- renderPlot({
        
        dtaAll <- selectedDta1()
        
        #create a subset of the data for the particular indicator in the loop
        loopdata <- subset(dtaAll, dtaAll$Indicator == Indicators1[my.i])
        
        #split this data into the two LAs selected
        loopdataCPP1 <- filter(loopdata, CPP == input$LA1)
        loopdataCPP2 <- filter(loopdata, CPP == input$CompLA1)
        
        #store unique values of "high is positive?" to use in test later
        HighValue <- unique(loopdata$`High is Positive?`)
        
        #create an if statement to determine the colour of the dot
        #need to create 2 statements to distinguish "high is positive"
        #compares whether the value of the authority is higher than the comparator and whether the improvement rate is higher
        coloursDotPos <- if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) & 
                                    (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                                 "green",
                                 if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) &
                                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                                         "yellow",
                                         if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                                                    (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                                                 "yellow",
                                                 if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                                                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                                                         "red",
                                                         "black"))))
        
        
        coloursDotNeg <- if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) & 
                                    (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                                 "red",
                                 if_else(((last(loopdataCPP1$value)) > (last(loopdataCPP2$value)) &
                                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                                         "yellow",
                                         if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                                                    (last(loopdataCPP1$Improvement_Rate)) > (last(loopdataCPP2$Improvement_Rate))),
                                                 "yellow",
                                                 if_else(((last(loopdataCPP1$value)) < (last(loopdataCPP2$value)) &
                                                            (last(loopdataCPP1$Improvement_Rate)) < (last(loopdataCPP2$Improvement_Rate))),
                                                         "green",
                                                         "black"))))
        
        
        #add new "year2" column to the data to store numeirc values for year
        loopdata <- arrange(loopdata, CPP)
        ##loopdata <- ddply(loopdata,. (CPP), transform, Year2 = (seq(1 : length(Year))))
        loopdata <- setDT(loopdata)[, Year2 :=(seq(1 : length(Year))), by = CPP]
        #add new "year3" column to store x axis labels
        ##loopdata <- ddply(loopdata,. (CPP), transform, Year3 = Year)
        loopdata <- setDT(loopdata)[, Year3 :=Year, by = CPP]
        loopdata$Year3 <- as.character(loopdata$Year3)
        Years2 <- unique(loopdata$Year2)
        
        #change year3 values so that labels will only show the 1st and last year
        loopdata$Year3[loopdata$Year2 > 1 & loopdata$Year2 < last(Years2)] <- ""
        
        #store unique year2 values and list of year3 values so that data length can be specified using these later
        Years3 <- filter(loopdata, CPP == input$LA1)
        Years3 <- Years3$Year3    
        
        #store raw data to be used for solid line
        dtaRaw <- loopdata[loopdata$Type == "Raw data",]        
        
        ggplot()+
          geom_line(data = loopdata,
                    aes(x = Year2, y = value, group = colourscheme, colour = colourscheme, linetype = "2"), lwd = 1, show.legend = FALSE)+
          geom_line(data = dtaRaw,
                    aes(x = Year2, y = value, group = colourscheme, colour = colourscheme, linetype = "1"), lwd = 1, show.legend = FALSE)+
          scale_color_manual(values = c("red", "blue"))+
          ggtitle(Indicators1[my.i])+
          annotate("text", x = Inf, y = Inf, label = sprintf('\U25CF'), size = 10, 
                   colour = (if_else(HighValue == "Yes", coloursDotPos, coloursDotNeg))
                   , hjust = 1, vjust = 1) +
          scale_x_continuous(breaks = c(1: length(Years2)), labels = Years3)+
          xlab("Year")+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour="black"),
                axis.text.x = element_text(vjust = 0.3))
        
      })
    })  
  }
  
  
  ##Events for Button inputs page 2 and 3======================  
  observeEvent(input$selAll2,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs2",
                                          selected = unique(CPPdta$Indicator))
               }, ignoreInit = TRUE 
  )
  observeEvent(input$selNone2,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs2",
                                          selected = NA)
               }, ignoreInit = TRUE      
  )
  observeEvent(input$selAll3,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs3",
                                          selected = unique(CPPdta$Indicator))
               }, ignoreInit = TRUE  
  )
  observeEvent(input$selNone3,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "grphs3",
                                          selected = NA)
               }, ignoreInit = TRUE      
  )
  
  #create single plot based on what indicator is selected===  
  output$Indi1Plot <- renderPlot({
    
    selectedDta1 <- selectedDta1()
    dtaAll<- selectedDta1
    dtasubset <- dtaAll[dtaAll$Indicator == input$Indi1,]
    
    #split this data into the two LAs selected
    dtasubsetCPP1 <- filter(dtasubset, CPP == input$LA1)
    dtasubsetCPP2 <- filter(dtasubset, CPP == input$CompLA1)
    
    #store unique values of "high is positive?" to use in test later
    HighValue <- unique(dtasubset$`High is Positive?`)
    
    #create an if statement to determine the colour of the dot
    #need to create 2 statement to distinguish "between positve high values"High is Positive"
    #compares whether the value of the authority is higher than the comparator and whether the improvement rate is higher
    coloursDotPos <- if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) & 
                                (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                             "green",
                             if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) &
                                        (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                                     "yellow",
                                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                                                (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                                             "yellow",
                                             if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                                                        (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                                                     "red",
                                                     "black"))))
    
    coloursDotNeg <- if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) & 
                                (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                             "red",
                             if_else(((last(dtasubsetCPP1$value)) > (last(dtasubsetCPP2$value)) &
                                        (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                                     "yellow",
                                     if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                                                (last(dtasubsetCPP1$Improvement_Rate)) > (last(dtasubsetCPP2$Improvement_Rate))),
                                             "yellow",
                                             if_else(((last(dtasubsetCPP1$value)) < (last(dtasubsetCPP2$value)) &
                                                        (last(dtasubsetCPP1$Improvement_Rate)) < (last(dtasubsetCPP2$Improvement_Rate))),
                                                     "green",
                                                     "black"))))
    
    #add new "year2" column to the data to store numeirc values for year
    dtasubset <- arrange(dtasubset, CPP)
    #dtasubset <- ddply(dtasubset,. (CPP), transform, Year2 = (seq(1 : length(Year))))
    dtasubset <- setDT(dtasubset)[, Year2:=seq(1:length(Year)), by = CPP]
    #add new "year3" column to store x axis labels
    #dtasubset <- ddply(dtasubset,. (CPP), transform, Year3 = Year)
    dtasubset <- setDT(dtasubset)[, Year3 :=Year, by = CPP]
    dtasubset$Year3 <- as.character(dtasubset$Year3)
    Years2 <- unique(dtasubset$Year2)
    
    #change year3 values so that labels will only show the 1st and last year
    dtasubset$Year3[dtasubset$Year2 > 1 & dtasubset$Year2 < last(Years2)] <- ""
    
    #store unique year3 values so that data length can be specified using these later
    Years3 <- filter(dtasubset, CPP == input$LA1)
    Years3 <- Years3$Year3    
    
    #store raw data to be used for solid line
    dtaRaw <- dtasubset[dtasubset$Type == "Raw data",]
    
    ggplot()+
      geom_line(data = dtasubset,
                aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "2"), lwd = 1, show.legend = FALSE)+
      geom_line(data = dtaRaw,
                aes(x = Year2, y = value, group = CPP, colour = CPP, linetype = "1"), lwd = 1, show.legend = FALSE)+
      ggtitle(input$Indi1)+
      annotate("text", x = Inf, y = Inf, label = sprintf('\U25CF'), size = 10,
               colour = (if_else(HighValue == "Yes", coloursDotPos, coloursDotNeg))
               , hjust = 1, vjust = 1)+
      scale_x_continuous(breaks = c(1: length(Years2)), labels = Years3)+
      xlab("Year")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour="black"),
            axis.text.x = element_text(vjust = 0.3))
  })
  
  
  ##Create Ui Outputs for page 2 & 3 =================    
  ##create all graphs that can be shown in Pages 3
  #These are then pulled through in the uiOutputs
  for(i in 1:18){
    local({
      my.i <- i
      nms <- gsub(" ", "",unique(CPPdta$Indicator))[[my.i]]
      plotname <- paste("plot", nms, sep ="_")
      output[[plotname]] <- renderPlot({
        indis <- unique(CPPdta$Indicator)
        slInd <- indis[[my.i]]
        ##Need to get this to select most recent year, since indicators have different periods  
        dat <- filter(CPPdta, Indicator == slInd & Year %in% c("2016/17", "2014-2016"))
        dat$slct <- ifelse(dat$CPP == input$LA3, "Sel1", "Other") 
        cmp <- filter(dat, CPP == input$CompLA3)$value
        ggplot(data = dat) +
          geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), 
                   stat = "identity", position = "dodge", width = 0.5) +
          scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
          ggtitle(slInd)+
          xlab("")+
          ylab("")+
          geom_hline(aes(yintercept = cmp)) +
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust =1, vjust = 0))
      })
    })  
  }
  
  ##Create Graphs for Page 2 - Similar Councils Only
  for(i in 1:18){
    local({
      my.i <- i
      nms <- gsub(" ", "",unique(CPPdta$Indicator))[[my.i]]
      plotnameFG <- paste("FGplot", nms, sep ="_")
      output[[plotnameFG]] <- renderPlot({
        indis <- unique(CPPdta$Indicator)
        slInd <- indis[[my.i]]
        #get family group of LA for looped indicator
        FGNo <- unique(filter(CPPdta, Indicator == slInd &  CPP == input$LA2)[[6]])
        ##Need to get this to select most recent year, since indicators have different periods  
        dat <- filter(CPPdta, Indicator == slInd & Year %in% c("2016/17", "2014-2016"))
        dat$slct <- ifelse(dat$CPP == input$LA2, "Sel1", "Other") 
        cmp <- filter(dat, CPP == input$CompLA2)$value
        dat <- filter(dat, FG == FGNo)
        ggplot(data = dat) +
          geom_bar(aes(x = reorder(CPP,-value), y = value, fill = slct), 
                   stat = "identity", position = "dodge", width = 0.5) +
          scale_fill_manual(values = c("blue","red"), breaks = c("Other", "Sel1")) +
          guides(fill = FALSE) +
          ggtitle(slInd)+
          geom_hline(aes(yintercept = cmp))+
          xlab("")+
          ylab("")+
          theme_bw()+
          theme(axis.text.x = element_text(angle =90, hjust = 1, vjust = 0))
      })
    })  
  }
  
  
  ##Render a UI with a certain number of rows and columns based on selected graphs
  output$uiPage3 <- renderUI({
    slctd <- length(input$grphs3)
    #number of columns is 4, unless there are less than 3 graphs
    cls <- if(slctd>3){4} else{slctd}
    #The percentage fo the space each columns will occupy
    pctCols <- 100/cls
    pctCols <- paste0(pctCols, "%")
    #number of rows is the number of graphs divided by 4 and rounded up eg 7 = 2 rows
    rows <- ceiling(slctd/4)
    ##Dynamically create plot height  
    pltheight <- ifelse(rows <2, "600px",ifelse(rows>4,"275px",paste0(900/rows, "px")))
    inptLst <- as.list(gsub(" ", "",input$grphs3))
    ##Create however many columns and then rows as needed
    fluidRow(
      #split into columns based on no. selected indicators
      column(12/cls,map(1, function(nc){
        #This part selects graphs created above depending on the 
        #number of indicators e.g if 12 the map function will pull out
        #1,5,9 using the seq function
        plot_output_list1<- map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm1 <- inptLst[[x]]
          plotname <- paste("plot", tstNm1, sep = "_")
          plotOutput(plotname, height = pltheight)
        })
        do.call(tagList, plot_output_list1)         
      }) ),  
      column(12/cls,map(2, function(nc){
        #this does the same thing as above, but selectes the next set of indicators
        #e.g. with 12 it goes 2,6,10
        #tryCatch is needed because there will be an error if the number of columns
        #is less than 2 => I need it to return nothing in this case
        plot_output_list2<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm2 <- inptLst[[x]]
          plotname <- paste("plot", tstNm2, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list2)         
      })
      ),
      column(12/cls,map(3, function(nc){
        plot_output_list3<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm3 <- inptLst[[x]]
          plotname <- paste("plot", tstNm3, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list3)         
      })
      ),
      column(12/cls,map(4, function(nc){
        plot_output_list4<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm4 <- inptLst[[x]]
          plotname <- paste("plot", tstNm4, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list4)         
      })
      )
      
    )  
  })
  
  ##Render a UI with a certain number of rows and columns based on selected graphs
  ##Except this one is for page 2!  
  output$uiPage2 <- renderUI({
    slctd <- length(input$grphs2)
    #number of columns is 4, unless there are less than 3 graphs
    cls <- if(slctd>3){4} else{slctd}
    #The percentage fo the space each columns will occupy
    pctCols <- 100/cls
    pctCols <- paste0(pctCols, "%")
    #number of rows is the number of graphs divided by 4 and rounded up eg 7 = 2 rows
    rows <- ceiling(slctd/4)
    ##Dynamically create plot height  
    pltheight <- ifelse(rows <2, "600px",ifelse(rows>4,"275px",paste0(900/rows, "px")))
    inptLst <- as.list(gsub(" ", "",input$grphs2))
    ##Create however many columns and then rows as needed
    fluidRow(
      #split into columns based on no. selected indicators
      column(12/cls,map(1, function(nc){
        #This part selects graphs created above depending on the 
        #number of indicators e.g if 12 the map function will pull out
        #1,5,9 using the seq function
        plot_output_list1<- map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm1 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm1, sep = "_")
          plotOutput(plotname, height = pltheight)
        })
        do.call(tagList, plot_output_list1)         
      }) ),  
      column(12/cls,map(2, function(nc){
        #this does the same thing as above, but selectes the next set of indicators
        #e.g. with 12 it goes 2,6,10
        #tryCatch is needed because there will be an error if the number of columns
        #is less than 2 => I need it to return nothing in this case
        plot_output_list2<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm2 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm2, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list2)         
      })
      ),
      column(12/cls,map(3, function(nc){
        plot_output_list3<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm3 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm3, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list3)         
      })
      ),
      column(12/cls,map(4, function(nc){
        plot_output_list4<- tryCatch(map(seq(from = nc,to = slctd,by = cls), function(x){
          tstNm4 <- inptLst[[x]]
          plotname <- paste("FGplot", tstNm4, sep = "_")
          plotOutput(plotname, height = pltheight)
        }), 
        error=function(cond) {
          
          return(list())
        }
        )
        do.call(tagList, plot_output_list4)         
      })
      )
      
    )  
  })
  
  #Create Ui ouputs for page 4 - My communities page=============  
  
  #create reactive input that updates indicator selection to select all or clear all  
  observeEvent(eventExpr = input$IndiAll,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "Indi4",
                                          selected = unique(IGZdta$Indicator))
               }
  )
  
  observe({
    if(input$IndiClear >0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "Indi4",
                               selected = character(0))
    }
  })  
  
  ######Create table output 
  output$MyCommunitiesTbl <- DT::renderDataTable({
    
    
    ###Create rankings for outcomes
    IGZBest <- filter(IGZ1617, CPP %in% input$LA4 & Indicator %in% input$Indi4)
    
    #Calculate combined CPP score and combined Type score by grouping by individial IGZ and summing scores
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, CombinedCPPScore = (sum(CPPScore)))
    IGZBest <-setDT(IGZBest)[, CombinedCPPScore := sum(CPPScore), by = InterZone]
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, CombinedTypeScore = (sum(TypeScore)))
    IGZBest <-setDT(IGZBest)[, CombinedTypeScore := sum(TypeScore), by = InterZone]
    
    #Filter data so that combined scores are only displayed once for each IGZ
    #add column which displays the name of the 1st indicator selected, then filter where data equals this
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, FilterRef = (first(Indicator)))
    IGZBest <- setDT(IGZBest)[, FilterRef:= first(Indicator), by = InterZone]
    IGZBest <- filter(IGZBest, Indicator == FilterRef)
    
    #Create rankings for scores
    IGZBest$CPPScoreRank <- rank(IGZBest$CombinedCPPScore)
    IGZBest$TypeScoreRank <- rank(IGZBest$CombinedTypeScore)
    
    
    ###Create rankingsfor improvement 
    IGZImprovement <- filter(IGZChange, CPP %in% input$LA4 & Indicator %in% input$Indi4)
    
    #Calculate combined CPP score and combined Type score by grouping by individial IGZ and summing scores
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, CombinedCPPChangeScore = (sum(CPPChangeScore)))
    IGZImprovement <- setDT(IGZImprovement)[,CombinedCPPChangeScore := sum(CPPChangeScore), by = InterZone]
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, CombinedTypeChangeScore = (sum(TypeChangeScore)))
    IGZImprovement <- setDT(IGZImprovement)[,CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    
    #Filter data so that combined scores are only displayed once for each IGZ
    #add column which displays the name of the 1st indicator selected, then filter where data equals this
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, FilterRef = (first(Indicator)))
    IGZImprovement <- setDT(IGZImprovement)[, FilterRef := first(Indicator), by = InterZone]
    IGZImprovement <-filter(IGZImprovement, Indicator == FilterRef)
    
    #Create rankings for scores
    IGZImprovement$CPPChangeRank <- rank(IGZImprovement$CombinedCPPChangeScore)
    IGZImprovement$TypeChangeRank <- rank(IGZImprovement$CombinedTypeChangeScore)
    
    
    ###Split Data into 4 individual DataTables for each ranking, then combine into 1 table
    
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
    
    ###Calculate References for Colours
    #Store the number of IGZ
    NoIGZ <- nrow(MyCommunitiesDta)
    NoIGZ <- as.numeric(NoIGZ)
    
    #select the number of colours required
    Clrs <- if_else((NoIGZ < 11),NoIGZ,11)
    
    #Divide the number of IGZ by the number of colours being used to determine how many times to repeat colour 
    groupings <- round(NoIGZ/Clrs)
    #Create a number sequence for the different colours
    Number_seq <- rep(1:Clrs, each = groupings)
    #Check the length of this colour sequence to determine whether more needs to be added or some need to be removed
    length_seq <- length(Number_seq)
    Diff_seq <- NoIGZ - length_seq
    
    ##Add in if statement that checks whether Diff_seq is negative 
    #if difference is negative have a smaller number within each grouping
    if(Diff_seq < 0) {groupings <- groupings -1}
    
    #Create the number sequence again on this bases
    Number_seq2 <- rep(1:Clrs, each = groupings)
    length_seq2 <- length(Number_seq2)
    Diff_seq2 <- NoIGZ - length_seq2
    
    #distribute a roughly equal proportion of the colours
    extra <- seq.int(from = 2, to = Clrs, length.out = Diff_seq2)
    extra <- round(extra)
    
    #add this to the overall sequence, order it and add to the data set
    Complete_seq <- c(Number_seq2,extra)
    Complete_seq <- sort(Complete_seq)
    MyCommunitiesDta$Helper1 <- Complete_seq
    
    ####Create colours for the remaining columns
    #Filter the namesin column 1 and the colour references of these
    #rename the columns and join these back up with the relevant column in the original table
    #This will keep the order of the original column but match the colour references to the IGZ name
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
    
    #Store colours to be used
    ColourPal <- brewer.pal(Clrs,"RdYlGn")
    
    #Call CPP Name to be used in variable names
    CPPName <- input$LA4
    
    #Rename variables
    colnames(MyCommunitiesDta)[1] <- paste("Within ", CPPName, " which communities have the poorest outcomes?")
    colnames(MyCommunitiesDta)[2] <- paste("Compared to other, similar communities, how do those in ", 
                                           CPPName, " fare? (are they better or worse than expected?)")
    colnames(MyCommunitiesDta)[3] <- paste("Within ", CPPName, " which communities have improved the least?")
    colnames(MyCommunitiesDta)[4] <- paste("Within ", CPPName, "which communities have improved the least relative 
                                           to other similar communities?")
    
    ##Store Column Names
    Container1 <- paste("Within ", CPPName, " which communities have the poorest outcomes?")
    Container2 <- paste("Compared to other, similar communities, how do those in ", 
                        CPPName, " fare? (are they better or worse than expected?)")
    Container3 <- paste("Within ", CPPName, " which communities have improved the least?")
    Container4 <- paste("Within ", CPPName, "which communities have improved the least relative 
                        to other similar communities?")
    
    #####add 4 empty columns so that there is space in between each column in the table
    #order these bewteen each of the columns and ensure column name is blank
    MyCommunitiesDta[,ncol(MyCommunitiesDta)+1] <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,9,2,3,4,5,6,7,8)]
    MyCommunitiesDta[,ncol(MyCommunitiesDta)+1] <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,2,3,10,4,5,6,7,8,9)]
    MyCommunitiesDta[,ncol(MyCommunitiesDta)+1] <- NA
    MyCommunitiesDta <- MyCommunitiesDta[,c(1,2,3,4,5,11,6,7,8,9,10)]
    colnames(MyCommunitiesDta)[c(2,4,6)] <- ""
    
    
    #Store values of the colours which need to have white text
    WhiteTxt <- c(head(Store_unique1,2),tail(Store_unique1,2))
    TxtValue <- Store_unique1
    TxtValue <- if_else(TxtValue %in% WhiteTxt, "White", "Black")
    
    #####Allow table to be split into top/bottom 10 and top/bottom 5
    
    #Create an if statement to determine how many rows to split by if CPP has small no. of IGZ
    Top10Rows <- if_else(NoIGZ<20,
                         if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)+0.5 ),
                         10)
    
    Bottom10Rows <- if_else(NoIGZ<20,
                            if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)-0.5 ),
                            10)
    
    #Create seperate table of top 10, add an empty row, then add to seperate table of bottom 10
    Top10 <- head(MyCommunitiesDta,Top10Rows)
    Top10[nrow(Top10)+2,] <- NA
    Bottom10 <- tail(MyCommunitiesDta, Bottom10Rows)
    TopBottom10 <- rbind(Top10, Bottom10)
    
    #Same for top and bottom 5
    Top5Rows <- if_else(NoIGZ<10,
                        if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)+0.5 ),
                        5)
    
    Bottom5Rows <- if_else(NoIGZ<10,
                           if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)-0.5 ),
                           5)
    
    Top5 <- head(MyCommunitiesDta,Top5Rows)
    Top5[nrow(Top5)+2,] <- NA
    Bottom5 <- tail(MyCommunitiesDta, Bottom5Rows)
    TopBottom5 <- rbind(Top5, Bottom5)
    
    #Call display input
    Display <- input$View
    
    #Create if statements to select data based on display input
    if(Display == "Top/bottom 10") { MyCommunitiesDta <- TopBottom10}
    if(Display == "Top/bottom 5") {MyCommunitiesDta <- TopBottom5}
    
    #Create custom HTML to allow column headers to span multiple columns
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
    
    #Create table
    datatable(MyCommunitiesDta, options = list(
      columnDefs =list(list(visible = FALSE, targets = c(7,8,9,10)),
                       list(width = '400px', targets = c(0,2,4,6))),
      pageLength = 136, 
      dom = "t", 
      ordering = F
    ),
    container = sketch,
    class = 'compact',
    rownames = FALSE,
    selection = 'none')%>%
      formatStyle(columns = 1, valueColumns = 8 ,backgroundColor = styleEqual(Store_unique1,ColourPal))%>%
      formatStyle(columns = 3, valueColumns = 9 ,backgroundColor = styleEqual(Store_unique2,ColourPal))%>%
      formatStyle(columns = 5, valueColumns = 10,backgroundColor = styleEqual(Store_unique3,ColourPal))%>%
      formatStyle(columns = 7, valueColumns = 11,backgroundColor = styleEqual(Store_unique4,ColourPal))%>%
      formatStyle(columns = 1, valueColumns = 8, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 3, valueColumns = 9, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 5, valueColumns = 10, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 7, valueColumns = 11, color = styleEqual(Store_unique1,TxtValue))
    
    
    
  })
  
  ##Create Leaflet Maps=============================
  output$IZUI <- renderUI({
    selectizeInput("IZ", "", choices = sort(unique(CPPMapDta[CPPMapDta$council == input$CPP, 11])),
                   options = list(placeholder = "Select a Community",
                                  onInitialize = I('function() { this.setValue(""); }')))
  })
  
  clrs<-brewer.pal(7, "RdYlGn")
  povPal <- colorBin(rev(clrs), SpPolysDF@data$povDecs)
  tariffPal <- colorBin(clrs, SpPolysDF@data$tariffDecs)
  posPal <- colorBin(clrs, SpPolysDF@data$posDecs)
  benPal <- colorBin(rev(clrs), SpPolysDF@data$benDecs)
  crimePal <- colorBin(rev(clrs), SpPolysDF@data$crimeDecs)
  admisPal <- colorBin(rev(clrs), SpPolysDF@data$admisDecs)
  
  plydata<-reactive({
    desIZ<- which(CPPMapDta$council %in% input$CPP & CPPMapDta$IZname %in% input$IZ)
    IZ_dzs<-SpPolysDF[desIZ,]
  })
  
  #create the map
  output$newplot<-renderLeaflet({
    p<-leaflet(plydata())%>%
      # addProviderTiles("OpenStreetMap.HOT")%>% #Humanitarian OpenStreetMap if desired
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~povPal(`povDecs`), color = "black")
    return(p)
  })
  output$newplot2<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~tariffPal(`tariffDecs`),  color = "black")
    return(p)
  })
  output$newplot3<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~posPal(`posDecs`), color = "black")
    return(p)
  })
  output$newplot4<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~benPal(`benDecs`), color = "black")
    return(p)
  })
  output$newplot5<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~crimePal(`crimeDecs`), color = "black")
    return(p)
  })
  output$newplot6<-renderLeaflet({
    p<-leaflet(plydata())%>%
      addTiles()%>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~DataZone, fillColor = ~admisPal(`admisDecs`), color = "black")
    return(p)
  })
  
  ##Clickable popups for map1
  showDZPopup <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s",
              "Children in Poverty (%)", round(unique(selectedDZ[13]),2)), tags$br()
    ))
    leafletProxy("newplot") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot") %>% clearPopups()
    event <- input$newplot_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup(event$id, event$lat, event$lng)
    })
  })
  
  ##Clickable popups for map2
  showDZPopup2 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Tariff Score", round(unique(selectedDZ[14]),2)), tags$br()
    ))
    leafletProxy("newplot2") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot2") %>% clearPopups()
    event <- input$newplot2_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup2(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map3
  showDZPopup3 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Positive Destinations (%)", round(unique(selectedDZ[15]),2)), tags$br()
    ))
    leafletProxy("newplot3") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot3") %>% clearPopups()
    event <- input$newplot3_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup3(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map4
  showDZPopup4 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Out of Work Benefits (%)", round(unique(selectedDZ[16]),2)), tags$br()
    ))
    leafletProxy("newplot4") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot4") %>% clearPopups()
    event <- input$newplot4_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup4(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map5
  showDZPopup5 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "SIMD Crimes per 10,000", round(unique(selectedDZ[17]),2)), tags$br()
    ))
    leafletProxy("newplot5") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot5") %>% clearPopups()
    event <- input$newplot5_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup5(event$id, event$lat, event$lng)
    })
  })
  ##Clickable popups for map6
  showDZPopup6 <- function(group, lat, lng) {
    selectedDZ <- CPPMapDta[CPPMapDta$DataZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedDZ$DataZone))),
      sprintf("%s: %s\n",
              "Emergency Admissions per 100,000", round(unique(selectedDZ[18]),2)), tags$br()
    ))
    leafletProxy("newplot6") %>% addPopups(lng, lat, content, layerId = group)
  }
  
  #Makes the popups appear and clears old popups
  observe({
    leafletProxy("newplot6") %>% clearPopups()
    event <- input$newplot6_shape_click
    if (is.null(event))
      return()
    isolate({
      showDZPopup6(event$id, event$lat, event$lng)
    })
  })
  
  #Colours for Community Map
  communityPal <- colorBin(clrs, SpPolysIZ@data$rank_decs)
  
  #Subset IZ Data
  IZPlys <- reactive({
    
  })
  
  #Create Community Map
  output$communityMap <- renderLeaflet({
    sbst <- which(SpPolysIZ@data$council %in% input$CPPIZ)
    dt <- SpPolysIZ[sbst,]
    cp <- leaflet(dt) %>%
      addTiles() %>%
      addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~InterZone, fillColor = ~communityPal(`rank_decs`), color = "black")
  })
  #Add click function
  showIZPopup <- function(group, lat, lng){
    selectedIZ <- SpPolysIZ@data[SpPolysIZ@data$InterZone == group,]
    content <- as.character(tagList(
      tags$h4(as.character(unique(selectedIZ$`IGZ name`))),
      paste("Intermediate Geography Ranking:", as.character(unique(selectedIZ[14]))),
      tags$br()
    ))
    leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
  }
  #Make popup appear and clear old popups
  observe({
    leafletProxy("communityMap") %>% clearPopups()
    event <- input$communityMap_shape_click
    if(is.null(event)){
      return()}
    isolate({
      showIZPopup(event$id, event$lat, event$lng)
    })
    
  })
  
  
  ##Create Ui ouputs for page 5 - Community Profile=============    
  #Create reactive selection for community, filter to only show communities within the CPP
  output$Comm5 <- renderUI({
    IGZsubset <- filter(IGZdta, CPP == input$LA5)
    selectInput("Community5", "Select a Community", sort(unique(IGZsubset$InterZone_Name)))
  })
  
  #Create text showing group description of community selected
  output$Descrip <- renderText({
    IGZsubset <- filter(IGZdta, InterZone_Name == input$Community5)
    txt <- first(IGZsubset$Typology_Name)
    txt <- paste("Group Description: ", txt)
  })
  
  #Create text showing the number of other communities in the selected group
  output$GrpSize <- renderText({
    IGZsubset <- filter(IGZdta, InterZone_Name == input$Community5)
    Typology <- first(IGZsubset$Typology_Group)
    Indi <- first(IGZsubset$Indicator)
    Yr <- first(IGZsubset$Year)
    TypeSubset <- filter(IGZdta, Typology_Group == Typology & Indicator == Indi & Year == Yr)
    Size <- nrow(TypeSubset) -1
    txt <- paste(Size, " other, similar communities in this group")
  })
  
  
  ###create table output
  output$CommunityProfileTbl <- DT::renderDataTable({
    
    IGZsubset <- filter(IGZdta, InterZone_Name == input$Community5)
    Typology <- first(IGZsubset$Typology_Group)
    
    ###Create rankings for table
    ##Create Rankings for Outcomes
    IGZBest <- filter(IGZ1617, Typology_Group == Typology & Indicator %in% input$Indi5)
    
    #Calculate combined Type score by grouping by individial IGZ and summing scores
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, CombinedTypeScore = (sum(TypeScore)))
    IGZBest <- setDT(IGZBest)[,CombinedTypeScore := (sum(TypeScore)), by = InterZone]
    #Filter data so that combined scores are only displayed once for each IGZ
    #add column which displays the name of the 1st indicator selected, then filter where data equals this
    #IGZBest <- ddply(IGZBest,. (InterZone), transform, FilterRef = (first(Indicator)))
    IGZBest <- setDT(IGZBest)[,FilterRef := (first(Indicator)), by = InterZone]
    IGZBest <- filter(IGZBest, Indicator == FilterRef)
    
    #Create rankings for scores
    IGZBest$TypeScoreRank <- rank(IGZBest$CombinedTypeScore)
    
    #Concatenate CPP Names with Community Names
    IGZBest$InterZone_Name <-  paste(IGZBest$CPP, "-",IGZBest$InterZone_Name)    #IGZBest <- setDT(IGZBest)[, InterZone_Name := paste(CPP, "-",InterZone_Name), by = InterZone]
    ##Create Rankings for Improvement
    IGZImprovement <- filter(IGZChange, Typology_Group == Typology & Indicator %in% input$Indi5)
    
    #Calculate combined Type score by grouping by individial IGZ and summing scores
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, CombinedTypeChangeScore = (sum(TypeChangeScore)))
    IGZImprovement <- setDT(IGZImprovement)[, CombinedTypeChangeScore := sum(TypeChangeScore), by = InterZone]
    #Filter data so that combined scores are only displayed once for each IGZ
    #add column which displays the name of the 1st indicator selected, then filter where data equals this
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, FilterRef = (first(Indicator)))
    IGZImprovement <- setDT(IGZImprovement)[,FilterRef := first(Indicator), by = InterZone]
    IGZImprovement <-filter(IGZImprovement, Indicator == FilterRef)
    
    #Create rankings for scores
    IGZImprovement$TypeChangeRank <- rank(IGZImprovement$CombinedTypeChangeScore)
    
    #Concatenate CPP Names with Community Names
    #IGZImprovement <- ddply(IGZImprovement,. (InterZone), transform, InterZone_Name = paste(CPP, "-",InterZone_Name))
    IGZImprovement <-setDT(IGZImprovement)[, InterZone_Name := paste(CPP, "-",InterZone_Name), by = InterZone]
    ###Split Data into 2 individual DataTables for each ranking, then combine into 1 table
    Column1 <- select(IGZBest, c(InterZone_Name, TypeScoreRank)) %>%
      arrange(TypeScoreRank)
    colnames(Column1)[1] <- "Variable1"
    
    Column2 <- select(IGZImprovement, c(InterZone_Name, TypeChangeRank)) %>%
      arrange(TypeChangeRank)
    colnames(Column2)[1] <- "Variable2"
    
    CommunityProfileDta <- cbind(Column1, Column2) %>%
      select(c(-TypeScoreRank, -TypeChangeRank))
    
    ###Calculate References for Colours
    #Store the number of IGZ
    NoIGZ <- nrow(CommunityProfileDta)
    NoIGZ <- as.numeric(NoIGZ)
    
    #select the number of colours required
    Clrs <- if_else((NoIGZ < 11),NoIGZ,11)
    
    #Divide the number of IGZ by the number of colours being used to determine how many times to repeat colour 
    groupings <- round(NoIGZ/Clrs)
    #Create a number sequence for the different colours
    Number_seq <- rep(1:Clrs, each = groupings)
    #Check the length of this colour sequence to determine whether more needs to be added or some need to be removed
    length_seq <- length(Number_seq)
    Diff_seq <- NoIGZ - length_seq
    
    ##Add in if statement that checks whether Diff_seq is negative 
    #if difference is negative have a smaller number within each grouping
    if(Diff_seq < 0) {groupings <- groupings -1}
    
    #Create the number sequence again on this bases
    Number_seq2 <- rep(1:Clrs, each = groupings)
    length_seq2 <- length(Number_seq2)
    Diff_seq2 <- NoIGZ - length_seq2
    
    #distribute a roughly equal proportion of the colours
    extra <- seq.int(from = 2, to = Clrs, length.out = Diff_seq2)
    extra <- round(extra)
    
    #add this to the overall sequence, order it and add to the data set
    Complete_seq <- c(Number_seq2,extra)
    Complete_seq <- sort(Complete_seq)
    CommunityProfileDta$Helper1 <- Complete_seq
    
    ####Create colours for the remaining column
    #Filter the names in column 1 and the colour references of these
    #rename the columns and join these back up with the relevant column in the original table
    #This will keep the order of the original column but match the colour references to the IGZ name
    colours2 <- CommunityProfileDta[,c(1,3)]
    colnames(colours2) <- c("Variable2", "Helper2")
    CommunityProfileDta <- join(CommunityProfileDta, colours2, by = "Variable2")
    
    #Store unique colour reference to use as intervals in styling
    Store_unique1 <- unique(CommunityProfileDta$Helper1)
    Store_unique2 <- unique(CommunityProfileDta$Helper2) %>% sort
    
    #Store colours to be used
    ColourPal <- brewer.pal(Clrs,"RdYlGn")
    
    #Call CPP Name to be used in variable names
    CPPName <-  input$LA5
    
    ###Create helper column to determine which IGZ should be bold
    #call Community name
    Community <- input$Community5
    
    #concatenate community name with CPP name so that comparison can be made
    Community <- paste(CPPName, "-", Community)
    
    #Add new community name as a column of data
    CommunityProfileDta$Community <- Community
    
    #If IGZ name matches the community selected set as yes, otherwise no
    CommunityProfileDta$Community1 <- if_else(CommunityProfileDta$Variable1 == CommunityProfileDta$Community,
                                              "Yes","No")
    CommunityProfileDta$Community2 <- if_else(CommunityProfileDta$Variable2 == CommunityProfileDta$Community,
                                              "Yes","No")
    CommunityProfileDta <- select(CommunityProfileDta, -Community)
    
    #Store unique Yes No values for levels in style Equal
    fontlevels <- c("No", "Yes")
    
    #Store values of font wanted
    fontvalues <- c('normal', 'bold')
    
    #Rename variables
    colnames(CommunityProfileDta)[1] <- paste("How does the selected community in  ", CPPName, 
                                              " compare to similar communities in Scotland?")
    colnames(CommunityProfileDta)[2] <- paste("How does the improvement rate of the selected community in ", 
                                              CPPName, " compare to similar communities in Scotland?")
    
    #####add empty column so that there is space between the columns in the table
    #order these bewteen the columns and ensure column name is blank
    CommunityProfileDta[,ncol(CommunityProfileDta)+1] <- NA
    CommunityProfileDta <- CommunityProfileDta[,c(1,7,2,3,4,5,6)]
    colnames(CommunityProfileDta)[2] <- ""
    
    #Store values of the colours which need to have white text
    WhiteTxt <- c(head(Store_unique1,2),tail(Store_unique1,2))
    TxtValue <- Store_unique1
    TxtValue <- if_else(TxtValue %in% WhiteTxt, "White", "Black")
    
    #####Allow table to be split into top/bottom 10 and top/bottom 5
    
    #Create an if statement to determine how many rows to split by if CPP has small no. of IGZ
    Top10Rows <- if_else(NoIGZ<20,
                         if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)+0.5 ),
                         10)
    
    Bottom10Rows <- if_else(NoIGZ<20,
                            if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)-0.5 ),
                            10)
    
    #Create seperate table of top 10, add an empty row, then add to seperate table of bottom 10
    Top10 <- head(CommunityProfileDta,Top10Rows)
    Top10[nrow(Top10)+2,] <- NA
    Bottom10 <- tail(CommunityProfileDta, Bottom10Rows)
    TopBottom10 <- rbind(Top10, Bottom10)
    
    #Same for top and bottom 5
    Top5Rows <- if_else(NoIGZ<10,
                        if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)+0.5 ),
                        5)
    
    Bottom5Rows <- if_else(NoIGZ<10,
                           if_else((NoIGZ%%2)==0, NoIGZ/2, (NoIGZ/2)-0.5 ),
                           5)
    
    Top5 <- head(CommunityProfileDta,Top5Rows)
    Top5[nrow(Top5)+2,] <- NA
    Bottom5 <- tail(CommunityProfileDta, Bottom5Rows)
    TopBottom5 <- rbind(Top5, Bottom5)
    
    #Call display input
    Display <- input$View5
    
    #Create if statements to select data based on display input
    if(Display == "Top/bottom 10") { CommunityProfileDta <- TopBottom10}
    if(Display == "Top/bottom 5") {CommunityProfileDta <- TopBottom5}
    
    #Create table
    datatable(CommunityProfileDta, options = list(
      columnDefs =list(list(visible = FALSE, targets = c(3,4,5,6)),
                       list(width = '400px', targets = c(0,2))),
      pageLength = 136, 
      dom = "t", 
      ordering = F
    ),
    class = 'compact',
    rownames = FALSE,
    selection = 'none')%>%
      formatStyle(columns = 1, valueColumns = 4 ,backgroundColor = styleEqual(Store_unique1,ColourPal))%>%
      formatStyle(columns = 3, valueColumns = 5 ,backgroundColor = styleEqual(Store_unique2,ColourPal))%>%
      formatStyle(columns = 1, valueColumns = 4, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 3, valueColumns = 5, color = styleEqual(Store_unique1,TxtValue))%>%
      formatStyle(columns = 1, valueColumns = 6, fontWeight = styleEqual(fontlevels,fontvalues))%>%
      formatStyle(columns = 3, valueColumns = 7, fontWeight = styleEqual(fontlevels,fontvalues))
    
  })
  
  ####Create Graphs for Community Profile Page

  #Create ui output for checkbox selection
  output$LineChoices5 <- renderUI({
    Choices <- c(input$Community5, input$LA5, "Scotland", "Group Average")
    checkboxGroupInput("Choices5", "Select lines to plot", Choices, selected = Choices)
  })
  
  #Store indicators to be plotted
  Indicators5 <- unique(IGZdta$Indicator)
  
  ##use reactive functions to store possible data selections
  LineChoiceDta <- reactive({
    Community <- filter(IGZdta, InterZone_Name == input$Community5)
    Community$Identifier <- input$Community5
    Community$Colours <- "red"
    Community <- select(Community, c(-InterZone, -InterZone_Name, -CPP, -Typology_Group, -Typology_Name) )
 
    Indicators <- unique(IGZdta$Indicator)
    LA <- filter(CPPdta, CPP == input$LA5 & Indicator %in% Indicators)
    LA$Identifier <- input$LA5
    LA$Colours <- "green"
    LA <- select(LA, c(-CPP, -FG))

    Indicators <- unique(IGZdta$Indicator)
    Scotland <- filter(CPPdta, CPP == "Scotland" & Indicator %in% Indicators)
    Scotland$Identifier <- "Scotland"
    Scotland$Colours <- "blue"
    Scotland <- select(Scotland, c(-CPP, -FG))

    IGZsubset <- filter(IGZdta, InterZone_Name == input$Community5)
    Typology <- first(IGZsubset$Typology_Group)
    GrpAv <- filter(IGZdta, Typology_Group == Typology)
    #GrpAv <- select(GrpAv, -`High is Positive?`)
    GrpAv <- ddply(GrpAv,. (Indicator, Year), transform, GrpAver = mean(value))
    #GrpAv <- setDT(GrpAv)[,GrpAver := mean(value), by = list(Indicator, Year)]
    GrpAv <- filter(GrpAv, InterZone_Name == input$Community5)
    GrpAv <- select(GrpAv, -value)
    colnames(GrpAv)[9] <- "value"
    GrpAv$Identifier <- "Group Average"
    GrpAv$Colours <- "orange"
    GrpAv <- select(GrpAv, c(-InterZone, -InterZone_Name, -CPP, -Typology_Group, -Typology_Name))
    LineChoiceDta <- rbind(Community, LA, Scotland, GrpAv)
    })
  
  ###Create plot outputs
  for(i in seq_along(Indicators5)){
    local({
      my.i <- i
      plotname <- paste("5plot", my.i, sep ="_")
      output[[plotname]]<- renderPlot({
        
        #Combine reactive data into one data set
        LineChoiceDta <- LineChoiceDta()
        #Add column to data to fix y axis labels
        LineChoiceDta$YearLabels <- LineChoiceDta$Year
        LineChoiceDta$YearLabels <- if_else(LineChoiceDta$Year == "2006/07","2006/07",
                                            if_else(LineChoiceDta$Year == "2016/17", "2016/17",
                                                    if_else(LineChoiceDta$Year == "2020/21","2020/21","")))
        #LineChoiceDta <- ddply(LineChoiceDta,. (Indicator, Identifier), transform, YearPoints = (seq(1 : length(Year))))
        LineChoiceDta <- setDT(LineChoiceDta)[,YearPoints := seq(1 : length(Year)), by = list(Indicator, Identifier) ]
        #filter this data to match choices selected
        LineChoiceDta <- filter(LineChoiceDta, Identifier %in% input$Choices5)
        
        #Create if statement that filters data based on projection selection and plots based on this
        if(input$Projections5 == "No") {LineChoiceDta <- filter(LineChoiceDta, Type != "Projected")} 
        
        #Subset data to plot the selected indicator within the loop
        loopdata <- filter(LineChoiceDta, Indicator == Indicators5[my.i])
        #Store unique colour values
        LineColours <- unique(loopdata$Colours)
        #Store unique year values
        YPoints <- unique(loopdata$YearPoints)
        YPoints <- as.numeric(YPoints)
        FilterRef <- first(loopdata$Identifier)
        YLabels <- filter(loopdata, Identifier == FilterRef)
        YLabels <- YLabels$YearLabels
        
        #Seperarate projected data so this can be plotted seperately
        DashedLine <- loopdata
        SolidLine <- filter(loopdata, Type != "Projected")
        
        
        #Create Plot
        ggplot()+
          geom_line(data = DashedLine, 
                    aes(x = YearPoints, y = value, group = Identifier, colour = Identifier, linetype = "2"),lwd = 1, show.legend = FALSE)+
          geom_line(data = SolidLine, 
                    aes(x = YearPoints, y = value, group = Identifier, colour = Identifier, linetype = "1"),lwd = 1, show.legend = FALSE)+
          ggtitle(Indicators5[my.i])+
          scale_colour_manual(breaks = LineColours, values = LineColours)+
          scale_x_continuous(breaks = c(1: length(YPoints)), labels = YLabels)+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour="black"),
                axis.text.x = element_text(vjust = 0.3))
        
      })
    })
  }
  
  
  ##All communities per indicator==================================
  ##Firstly render all of the plots for filling in the UI rendered below 
  myheight <- function(){
    nrow(unique(IGZdta[IGZdta$CPP== input$`CPP-AllC`,"InterZone_Name"]))*60
  }
  output$AllCPlots <- renderPlot({
    dta <- IGZdta[IGZdta$CPP== input$`CPP-AllC` & IGZdta$Indicator==input$`Indi-AllC`&IGZdta$Type != "Projected",c(2,8,9)]
    nComs <- length(unique(dta$InterZone_Name))
    comList <- unique(dta$InterZone_Name)
    dta2 <- CPPdta[CPPdta$CPP %in% input$`CPP-AllC`& CPPdta$Indicator==input$`Indi-AllC`&CPPdta$Type != "Projected",c(1,4,5)]
    dta3 <- CPPdta[CPPdta$CPP %in% "Scotland"& CPPdta$Indicator==input$`Indi-AllC`&CPPdta$Type != "Projected",c(1,4,5)]
    colnames(dta2) <- colnames(dta)
    colnames(dta3) <- colnames(dta)
    dta <- rbind(dta, dta2, dta3)
    dta$colourscheme <-ifelse(dta$InterZone_Name == "Scotland","Scot",ifelse(dta$InterZone_Name == input$`CPP-AllC`,"CPP","Com"))
    yrs <- c(dta$Year[[1]], dta$Year[[length(dta$Year)]])
    ##lapply to generate plots
    plts <- list()
    plts <-lapply(1:nComs, FUN = function(.x){
      ggplot(data = dta[dta$InterZone_Name %in% c(comList[.x], input$`CPP-AllC`, "Scotland"),])+
        geom_line(aes(x = Year, y = value, group = colourscheme, colour = colourscheme), size = 1.5)+
        theme_bw()+
        ggtitle(comList[.x])+
        theme(axis.text.x =  element_text(angle = 90, vjust = 0, hjust = 1))+
        ylab("")+xlab("")+
        scale_x_discrete(breaks = yrs, expand = c(0.01,0.01))+
        scale_color_manual(breaks = c("Com", "CPP", "Scot") ,values = c("red", "green","blue"))+
        guides(colour = FALSE)
    })
    do.call("plot_grid", c(plts, ncol = 4))
  }, height = myheight)
  
})
