#ToDo:
#Integrate plot.ly as choice for output for export (given username and authentication key).
#find way to use multiple columns in plot.ly legend.
#Color code input types + collapsable for cleaner UI.
#Add help and documentation to web page
#Add plots of growth rate/doubling time as function of log-OD
#Add plots for expression levels
#Add interactive graphs via highcharts

library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)
library(zoo)
library(plotly)

labelSubset <- function(df,label) {
  readings <- as.numeric(rownames(df[df[,1] == "Cycle Nr.",]))
  endings <- as.numeric(rownames(df[df[,1] == "End Time:",]))
  labelRow <- 0
  if(adjecantLabels(df)) {
    labelRow <-as.numeric(rownames(df[df[,1]==label,]))
    endings <- as.numeric(rownames(df[df[,2] == "",]))
  } else {
    labelRow <- as.numeric(rownames(df[df[,1]==paste0("Label: ",label),]))
  }
  fromRow <- readings[readings > labelRow][1]
  endRow <- endings[endings>fromRow][1]
  return(df[(fromRow):(endRow-1),])
}

getPlateByLabel <- function(df,label) {
  plotData <- labelSubset(df,label)
  if(dataInRows(plotData)) {
    print("data in rows")
    firstcol <- plotData[,1] # The first column contains the descriptors of the rows
    plotData <- as.data.frame(t(plotData)) # Get the data to be in columns form
    colnames(plotData) <- firstcol # Set the proper column names
  } else {
    print("data in columns")
    colnames(plotData) <- plotData[1,]
  }
  irrelevantCols <- colnames(plotData)[grep("^Cycle|^Temp|^O2|^CO2",colnames(plotData))]
  plotData <- plotData[,!(names(plotData) %in% irrelevantCols)]
  plotData <- plotData[-1,] # Omit headings row
  plotData <- na.omit(plotData)
  colnames(plotData)[1] <- "Time" #Rename the time column so that it has a "nicer" name
  plotData[] <- lapply(plotData,as.character) # Convert values to numeric
  plotData[] <- lapply(plotData,as.numeric)
  plotData[,"Time"] = plotData[,"Time"]/3600 # convert time to hours
  return(plotData)
}

dataInRows <- function(df) {
  return(df[2,1] != "1")
}

adjecantLabels <- function(df) {
  return(length(df[grep("^Label: ",df[,1]),1]) == 0)
}

getReaderData <- function(df) {
  df[df=="OVER"] <- "71000"
  labels <- df[grep("^Label: ",df[,1]),1]
  labels <- substring(labels,8)
  if(adjecantLabels(df)) {
    labels <- as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1]) #in this version labels are two lines above measurement tables
  }
  print(labels)
  measurements <- vector(mode = "list", length = length(labels))
  names(measurements) <- labels
  for (label in labels) {
    measurements[[label]] <- getPlateByLabel(df,label)
  }
  return(measurements)  
}
max_plots <- 10

shinyServer(function(input,output) {
  Data <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    df <- read.xls(inFile$datapath,stringsAsFactors=FALSE,header=FALSE)
    version <- df[1,]
    version <- version[version != ""]
    version <- version[!is.na(version)]
    version <- version[2]
    print(version)
    if(substring(version,1,15) == 'Tecan i-control') {
      return(getReaderData(df))
    } else { 
      return(NULL)
    }
  })
  
  wells <- reactive({
    if(is.null(Data())) { return() }
    
    plotData <- Data()[[names(Data())[1]]]
    cols <- c(colnames(plotData))
    cols <- cols[cols != "Time"]
    
    if(input$wellsToAnalyse == "Wells") {
      if(is.null(input$wells)) { return() }
      cols <- strsplit(input$wells,',')
      cols <- cols[[1]]
    }
    if(input$wellsToAnalyse == "Row") {
      if(is.null(input$row)) { return() }
      cols <- cols[grep(input$row,cols)]
    }
    if(input$wellsToAnalyse == "Column") {
      if(is.null(input$column)) { return() }
      cols <- cols[grep(paste0("[A-Z]",input$column,"$"),cols)]      
    }
    return(cols)
  })
  
  backgroundVals <- reactive({
    if(is.null(Data())) { return() }
    if(is.null(input$label)) { return() }
    wellsData <- Data()[[input$label]]
    
    if(input$backgroundMethod == "Manual value") {
      blankVals <- as.numeric(input$manualBackground)
    }
    else if(input$backgroundMethod == "Average of first measurements of well") {
      blankVals <- lapply(wellsData[],function(x) {return (mean(head(x,as.numeric(input$perWellMesNum))))})
    }
    else if(input$backgroundMethod == "Time average of blank wells") {
      blankWells <- strsplit(input$blankWells,',')[[1]]
      blankVals <- mean(colMeans(wellsData[,blankWells,drop=FALSE]))
    }
    else if(input$backgroundMethod == "Point-wise average of blank wells") {
      blankWells <- strsplit(input$blankWells,',')[[1]]
      blankVals <- rowMeans(wellsData[,blankWells,drop=FALSE])
    }
    return(blankVals)
  })
  
  backgroundSubtracted <- reactive({
    plotData <- Data()[[input$label]]
    cols <- wells()
    wellsData <- plotData[,cols,drop=FALSE]
    blankVals <- backgroundVals()
    for(col in cols) {
      if(col %in% names(blankVals)) {
        wellsData[,col] <- wellsData[,col]-as.numeric(blankVals[col])        
      } else {
        wellsData[,col] <- wellsData[,col]-blankVals
      }
    }      
    wellsData[,"Time"] <- plotData$Time
    return(wellsData)
  })
  
  backgroundSubtractedLog <- reactive({
    bgSubtracted <- backgroundSubtracted()
    print(wells())
    bgSubtracted[,wells()] <- lapply(bgSubtracted[,wells()],log)
    return(bgSubtracted)
  })  
  
  timeLimits <- reactive({
    plotData <- Data()[[input$label]]
    timesRange <- range(plotData$Time)
    delta <- timesRange[2]-timesRange[1]
    return(c(timesRange[1]-0.05*delta,timesRange[2]+0.05*delta))
  })
  
  rollingWindowRegression <- reactive({
    reg <- function(window) {
      cols <- colnames(window)
      cols <- cols[cols!="Time"]
      window <- as.data.frame(window)
      res <- list()
      for(col in cols) {
        data <- window[,col]
        data[which(!is.finite(data))] = NA
        val <- mean(data[! is.na(data)])
        times <- window[,"Time"]
        times <- times[!is.na(data)]
        data <- data[!is.na(data)]
        if(length(data)>1) {
          c <- lm(data ~ times)
          rsq = summary(c)$r.squared
          if(is.na(rsq) || rsq < as.numeric(input$rsquare)) {
            res[col] <- NA
          } else {
            res[col] <- (summary(c)$coefficients)["times","Estimate"]
          }
          res[paste0(col,'.std_err')]<-summary(c)$coefficients["times","Std. Error"]  
        } else {
          res[col] <- NA
          res[paste0(col,'.std_err')]<- NA
        }
        res[paste0(col,'.vals')]<- val
      }
      return(res)
    }
    
    wellsData <- backgroundSubtractedLog()
    windowSize <- as.numeric(input$windowSize)
    
    regs <- rollapply(wellsData,width=windowSize,reg,by.column=FALSE)
    regs <- as.data.frame(regs,stringsAsFactors = FALSE)
    regs[,"Time"] <- as.numeric(head(wellsData$Time,length(regs[,1])))
    for(col in colnames(regs)) {
      regs[,col] <- as.numeric(regs[,col])
    }    
    
    std_errs = grep("std_err$",colnames(regs),value=TRUE)
    vals = grep("vals$",colnames(regs),value=TRUE)
    
    regs.long <- melt(regs,id.vars=c(std_errs,vals,"Time"))# Reformat the data to be appropriate for multi line plot
    regs.long[,'se']=1
    regs.long[,'val']=1
    for (col in wells()) {
      regs.long[which(regs.long$variable==col),'se']=regs.long[which(regs.long$variable==col),paste0(col,".std_err")]
      regs.long[which(regs.long$variable==col),'val']=regs.long[which(regs.long$variable==col),paste0(col,".vals")]
    }
    return(regs.long)
  })
  
  output$fileUploaded <- reactive ({
    return (! is.null(Data()))
  })
  
  outputOptions(output,'fileUploaded',suspendWhenHidden=FALSE)

  output$labelSelect <- renderUI({ 
    if(is.null(Data())) { return() }
    selectInput(
    inputId = "label",
    label = "Measurement label to display",
    choices = names(Data()),
    selected = 1)
  })
  
  output$rowSelect <- renderUI({
    if(is.null(Data())) { return() }
    plotData <- Data()[[names(Data())[1]]]
    rows <- levels(factor(substring(colnames(plotData)[-1],1,1)))
    selectInput(
    inputId = "row",
    label = "Select row to display",
    choices = rows,
    selected = 1)
  })
  
  output$columnSelect <- renderUI({
    if(is.null(Data())) { return() }
    plotData <- Data()[[names(Data())[1]]]
    columns <- levels(factor(substring(colnames(plotData)[-1],2)))
    selectInput(
      inputId = "column",
      label = "Select column to display",
      choices = columns,
      selected = 1)
  })
  
  output$plots <- renderUI({
    if(is.null(Data())) { return() }
    if(input$analysisType == "Plate overview") {
      plotsNum = length(names(Data()))
    }
    if(input$analysisType == "Growth rate analysis") {
      plotsNum = length(input$grPlots)
    }
    # for each label - create a plotOutput object with the appropriate name:
    plots_list <- lapply(1:plotsNum,function(i){
      plotname <- paste0("plot",i)
      plotOutput(plotname)
    })
    do.call(tagList,plots_list)    
  })
  
  for (i in 1:max_plots) { # generate plots for all the labels (up to 10 labels are allowed)
    local({
      my_i <- i
      output[[paste0("plot",my_i)]] <- renderPlot({
        if(is.null(wells())) { return() }
        if(input$analysisType == "Plate overview") {
          label <- names(Data())[my_i]
          plotData <- Data()[[label]]    
          plotData <- plotData[,c("Time",wells())]
      
          ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
          ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))+geom_point()+geom_line()+
            guides(colour=guide_legend(title="Well",nrow=20))+
            xlab("time [h]")+
            ylab(label)
        }
        else if(input$analysisType =="Growth rate analysis") {
          if(my_i>length(input$grPlots)) {return ()}
          return(plots[[input$grPlots[my_i]]]())
        }
      })
    })  
  }
  
  output$plot <- renderUI({   
    if(is.null(wells())) { return() }
    plotData <- Data()[[input$label]]    
    plotData <- plotData[,c("Time",wells())]
    
    ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
    plt <- ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))+
      geom_point()+geom_line()+
      guides(colour=guide_legend(title="Well",nrow=20))+
      xlab("time [h]")+
      ylab(input$label)
    print("connecting")
    py <- plotly("uri.barenholz","hvi3ma3m30","https://plot.ly")
    print(py)
    print("uploading")
    res <- py$ggplotly(plt,kwargs=list(filename="rawmes",fileopt="overwrite",auto_open=FALSE))
    print(res)
    tags$iframe(src=res$response$url,frameborder="0",height=400,width=650)
  })
  
  plots <- list()
  plots[["raw"]] <- reactive({
    if(is.null(wells())) { return() }
      label <- input$label
      plotData <- Data()[[label]]    
      plotData <- plotData[,c("Time",wells())]
      
      ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
      p <- ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))+geom_point()+geom_line()+
        guides(colour=guide_legend(title="Well",nrow=20))+
        xlab("time [h]")+
        ylab(label)
      return(p)
  })
  plots[["background subtracted"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtracted()
    
    ggplotdata <- melt(wellsData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))+
      geom_point()+geom_line()+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_x_continuous(limits=timeLimits())+
      xlab("time [h]")+
      ylab(input$label)
    
  })
  plots[["background subtracted log"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtractedLog()
    
    ggplotdata <- melt(wellsData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))+
      geom_point()+geom_line()+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_x_continuous(limits=timeLimits())+
      xlab("time [h]")+
      ylab(paste0("log ",input$label))
  })
  plots[["growth rate vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()    
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=value+se,ymin=value-se),width=.1,position=pd)
    }
    p <- p+geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_y_continuous(limits=c(-0.1,NA))+
      scale_x_continuous(limits=timeLimits())+
      xlab("time [h]")+
      ylab("growth rate")
    return(p)
  })
  plots[["growth rate vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=val,y=value,colour=variable,group=variable))
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=value+se,ymin=value-se),width=.1,position=pd)
    }
    p <- p+geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_y_continuous(limits=c(-0.1,NA))+
      xlab(paste0("log ",input$label))+
      ylab("growth rate")
    return(p)
  })
  plots[["doubling time vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()
    ggplotdata[,'ymax'] <- ggplotdata$value+ggplotdata$se
    ggplotdata[,'ymin'] <- ggplotdata$value-ggplotdata$se
    ggplotdata[,c('value','ymax','ymin')] <- lapply(ggplotdata[,c('value','ymax','ymin')],function(x) {log(2)*60/x})
  
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable,group=variable))
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    }    
    p <- p+geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_y_continuous(limits=c(-10,as.numeric(input$maxdtime)))+
      scale_x_continuous(limits=timeLimits())+
      xlab("time [h]")+
      ylab("doubling time [min]")
    return(p)  
  })
  plots[["doubling time vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()
    ggplotdata[,'ymax'] <- ggplotdata$value+ggplotdata$se
    ggplotdata[,'ymin'] <- ggplotdata$value-ggplotdata$se
    ggplotdata[,c('value','ymax','ymin')] <- lapply(ggplotdata[,c('value','ymax','ymin')],function(x) {log(2)*60/x})
    
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=val,y=value,colour=variable,group=variable))
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    }    
    p <- p+geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_y_continuous(limits=c(-10,as.numeric(input$maxdtime)))+
      xlab(paste0("log ",input$label))+
      ylab("doubling time [min]")
    return(p)  
  })
})
