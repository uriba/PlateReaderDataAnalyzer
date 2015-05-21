#ToDo:
#Integrate plot.ly as choice for output for export (given username and authentication key).
#find way to use multiple columns in plot.ly legend.
#allow selection of graph display on top of every graph (simple, interactive, plot.ly, table)
#Add update plots button?
#Color code input types + collapsable for cleaner UI.
#Add help and documentation to web page
#Add plots for expression levels
## selection of channels
## selection of background method per channel
## available graphs: fluorescence / od
## fluorescence change / od change (where the cells aim to)
## fluorescence change / od (rate of production).
#Set clear headlines for page according to analysis types
#tooltip help
#support plate layout in interactive graphs
#display (interactive?) plate layout
#support multiple plate layout styles/files
#add links to download/use sample files
#support error bars on interactive graphs.
#accelerate by using reactive melted data and manipulate it.

library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)
library(zoo)
library(plotly)
library(rCharts)
library(foreach)

source('tecanProcess.R')

max_plots <- 10

shinyServer(function(input,output) {
  MultifileData <- reactive({
    platesnum <- as.numeric(input$platesnum)
    if (is.na(platesnum)){
      return(NULL)
    } else {
      return(readMultiFiles(input$datafile$datapath,platesnum))
    }
  })

  output$MultiFile <- reactive ({ 
    inFile <- input$datafile
    return((!is.null(inFile)) && (length(inFile$datapath) > 1))
  })

  Data <- reactive({
    print(input$datafile)
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    if(length(inFile$datapath) == 1) {
      return(readSingleFile(inFile$datapath))
    } else {
      return(MultifileData()[[as.numeric(input$platenum)]])
    }
  })

  WellsDesc <- reactive({
    inFile <- input$descfile
    if (is.null(inFile))
      return(NULL)
    df <- read.xls(inFile$datapath,stringsAsFactors=FALSE,header=FALSE,colClasses="character")
    rownames(df) <- df[,1]
    colnames(df) <- df[1,]
    df <- df[-1,-1]
    labels <- c()
    for(row in rownames(df)) {
      for(col in colnames(df)) { 
        labels[paste0(row,col)] <- df[row,col]
      }
    }
    return(labels)
  })

  wells <- reactive({
    if(is.null(Data())) { return() }
    
    labelData <- Data()[[names(Data())[1]]] #get wells from first label
    cols <- colnames(labelData)
    cols <- cols[cols != "Time"]
    
    if(input$wellsToAnalyse == "Wells") {
      if(is.null(input$wells)) { return() }
      cols <- strsplit(input$wells,',')[[1]]
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
    if(is.null(input$label)) { return() }
    wellsData <- Data()[[input$label]]
    
    if(input$backgroundMethod == "Manual value") {
      blankVals <- as.numeric(input$manualBackground)
    }
    else if(input$backgroundMethod == "Average of first measurements of well") {
      blankVals <- lapply(wellsData[],function(x) {return (mean(head(x,as.numeric(input$perWellMesNum))))})
    }
    else {
      blankWells <- strsplit(input$blankWells,',')[[1]]
      if(input$backgroundMethod == "Time average of blank wells") {
          blankVals <- mean(colMeans(wellsData[,blankWells,drop=FALSE]))
      }
      else if(input$backgroundMethod == "Point-wise average of blank wells") {
        blankVals <- rowMeans(wellsData[,blankWells,drop=FALSE])
      }
    }
    return(blankVals)
  })
  
  backgroundSubtracted <- reactive({
    labelData <- Data()[[input$label]]
    cols <- wells()
    wellsData <- labelData[,wells(),drop=FALSE]
    blankVals <- backgroundVals()
    for(col in cols) {
      if(col %in% names(blankVals)) {
        wellsData[,col] <- wellsData[,col]-as.numeric(blankVals[col])        
      } else {
        wellsData[,col] <- wellsData[,col]-blankVals
      }
    }      
    wellsData[,"Time"] = labelData$Time
    return(wellsData)
  })
  
  backgroundSubtractedLog <- reactive({
    bgSubtracted <- backgroundSubtracted()
    bgSubtracted[,wells()] <- lapply(bgSubtracted[,wells(),drop=FALSE],log)
    return(bgSubtracted)
  })  
  
  timeLimits <- reactive({
    labelData <- Data()[[input$label]]
    timesRange <- range(labelData$Time)
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
        times <- window[,"Time"]
        times <- times[is.finite(data)]
        data <- data[is.finite(data)]
        res[paste0(col,'.vals')]<-  mean(data)
        res[col] <- NA
        res[paste0(col,'.std_err')]<- NA
        if(length(data)>1) {
          c <- lm(data ~ times)
          rsq = summary(c)$r.squared
          if(!is.na(rsq) && rsq > as.numeric(input$rsquare)) {
            res[col] <- (summary(c)$coefficients)["times","Estimate"]
          }
          res[paste0(col,'.std_err')]<-summary(c)$coefficients["times","Std. Error"]  
        }
      }
      return(res)
    }
    
    wellsData <- backgroundSubtractedLog()
    windowSize <- as.numeric(input$windowSize)
    
    regs <- rollapply(wellsData,width=windowSize,reg,by.column=FALSE)
    regs <- as.data.frame(regs,stringsAsFactors = FALSE)
    regs[,"Time"] <- as.numeric(head(wellsData$Time,length(regs[,1])))
    regs[] = lapply(regs,as.numeric)
    
    std_errs = grep("std_err$",colnames(regs),value=TRUE)
    vals = grep("vals$",colnames(regs),value=TRUE)
    
    regs.long <- melt(regs,id.vars=c(std_errs,vals,"Time"))# Reformat the data to be appropriate for multi line plot
    regs.long[,c('se','val')]=1 #assign standard error and value of measurement where regression was taken
    for (col in wells()) {
      regs.long[which(regs.long$variable==col),'se']=regs.long[which(regs.long$variable==col),paste0(col,".std_err")]
      regs.long[which(regs.long$variable==col),'val']=regs.long[which(regs.long$variable==col),paste0(col,".vals")]
    }
    regs.long$ymax <- regs.long$value+regs.long$se
    regs.long$ymin <- regs.long$value-regs.long$se
    regs.long$label <- regs.long$variable
    wellsDesc <- WellsDesc()
    if(! is.null(wellsDesc)) {
        regs.long$label = wellsDesc[regs.long$variable]
    }
    return(regs.long)
  })

  dtRegression <- reactive({
    plotData <- rollingWindowRegression()
    plotData[,c('value','ymax','ymin')] <- lapply(plotData[,c('value','ymax','ymin')],grToDt)
    return (plotData)
  })

  output$fileUploaded <- reactive ({
    return (! is.null(Data()))
  })
  
  outputOptions(output,'fileUploaded',suspendWhenHidden=FALSE)
  outputOptions(output,'MultiFile',suspendWhenHidden=FALSE)

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
                         plotstylename <- paste0("plotstyle",i)
                         plot = plotOutput(paste0("plot",i))
                         if(plotstylename %in% names(input) && input[[plotstylename]] == "interactive") {
                             plot = showOutput(paste0("chart",i),"highcharts")
                           }
      tagList(uiOutput(plotstylename), plot)
    })
    do.call(tagList,plots_list)    
  })

  simplePlot <- function(label,data) {
    pd = position_dodge(0.1)
    return (ggplot(data=data,aes(x=Time,y=value,colour=label,group=variable))+
      geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      xlab("time [h]")+
      ylab(label))
  }

  simpleChart <- function(label,data,wellsDesc) {
    data <- data[is.finite(data$value),]
    p <- hPlot(x='Time',y='value', data=data, group='variable',type='scatter')
    p$chart(zoomType="xy")
    p$exporting(enabled=T)
    p$legend(layout='vertical',align="right",verticalAlign='top')
    colorNum <- length(wellsDesc)
    if(is.null(wellsDesc))
      colorNum <- length(unique(data$variable))
    p$colors(gg_color_hue(colorNum))
    p$plotOptions(scatter = list(lineWidth=1,marker=list(radius=8,symbol='circle')))
    p$xAxis(title=list(text="time [h]"))
    p$yAxis(title=list(text=label))
    return(p)
  }

  rawPlot <- function(label,plotData,wellsDesc) {
      ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
      ggplotdata$label <- ggplotdata$variable
      if(! is.null(wellsDesc)) {
        ggplotdata$label = wellsDesc[ggplotdata$variable]
      }
      return(simplePlot(label,ggplotdata))
  }

  rawChart <- function(label,plotData,wellsDesc) {
      ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
      ggplotdata$label <- ggplotdata$variable
      if(! is.null(wellsDesc)) {
        ggplotdata$label = wellsDesc[ggplotdata$variable]
      }
      return(simpleChart(label,ggplotdata,wellsDesc))
  }

   for (i in 1:max_plots) {
     local({
       my_i <- i
      plotstylename <- paste0("plotstyle",my_i)
      output[[plotstylename]] <- renderUI({
        selectInput(
                inputId = plotstylename,
                label = "Graph display",
                choices = c("standard","interactive"),
                selected = input[[plotstylename]])})
     })
   }

 
  for (i in 1:max_plots) { # generate plots for all the labels (up to 10 labels are allowed)
    local({
      my_i <- i
      output[[paste0("plot",my_i)]] <- renderPlot({
        if(is.null(wells())) { return() }
        if(input$analysisType == "Plate overview") {
          if(my_i>length(names(Data()))) {return ()}
          label <- names(Data())[my_i]
          plotData <- Data()[[label]]    
          return(rawPlot(label,plotData[,c("Time",wells())],WellsDesc()))
        }
        else if(input$analysisType =="Growth rate analysis") {
          if(my_i>length(input$grPlots)) {return ()}
          return(plots[[input$grPlots[my_i]]]())
        }
      })
      output[[paste0("chart",my_i)]] <- renderChart({
        if(is.null(wells())) { return() }
        if(input$analysisType == "Plate overview") {
          if(my_i>length(names(Data()))) {return ()}
          label <- names(Data())[my_i]
          plotData <- Data()[[label]]    
          p <- rawChart(label,plotData[,c("Time",wells())],WellsDesc())
          p$set(dom=paste0("chart",my_i))
          return(p)
        }
        else if(input$analysisType =="Growth rate analysis") {
          if(my_i>length(input$grPlots)) {return ()}
          p <- charts[[input$grPlots[my_i]]]()
          p$set(dom=paste0("chart",my_i))
          return(p)
        }
      })
    })  
  }

 gg_color_hue <- function(n) {
     hues = seq(15, 375, length=n+1)
       hcl(h=hues, l=65, c=100)[1:n]
 }
#plot.ly chart
  output$plot <- renderUI({   
    if(is.null(wells())) { return() }
    if(is.null(input$label)) { return() }
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
  charts <- list()

  plots[["raw"]] <- reactive({
    if(is.null(wells())) { return() }
      label <- input$label
      plotData <- Data()[[label]]    
      return(rawPlot(label,plotData[,c("Time",wells())],WellsDesc()))
  })

  charts[["raw"]] <- reactive({
    if(is.null(wells())) { return() }
      label <- input$label
      plotData <- Data()[[label]]
      return(rawChart(label,plotData[,c("Time",wells())],WellsDesc()))
  })

  plots[["background subtracted"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtracted()
    return(rawPlot(input$label,wellsData,WellsDesc()) + scale_x_continuous(limits=timeLimits()))
  })

  charts[["background subtracted"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtracted()
    p <-rawChart(input$label,wellsData,WellsDesc())
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    return(p)
  })

  plots[["background subtracted log"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtractedLog()
    return(rawPlot(paste0("log ",input$label),wellsData,WellsDesc()) + scale_x_continuous(limits=timeLimits()))
  })

  charts[["background subtracted log"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtractedLog()
    p <- rawChart(paste0("log ",input$label),wellsData,WellsDesc())
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    return(p)
  })

  plots[["growth rate vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()    
    pd = position_dodge(0.1)
    p <- simplePlot("growth rate",ggplotdata)
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    }
    p <- p+ scale_y_continuous(limits=c(-0.1,NA))+
      scale_x_continuous(limits=timeLimits())
    return(p)
  })

  charts[["growth rate vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()    
    p <- Highcharts$new()
    p$chart(zoomType="xy")
    p$exporting(enabled=T)
    p$legend(layout='vertical',align="right",verticalAlign='top')
    colorNum <- length(WellsDesc())
    if(is.null(WellsDesc()))
      colorNum <- length(unique(ggplotdata$variable))
    p$colors(gg_color_hue(colorNum))
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    p$yAxis(title=list(text="growth rate"),floor=-0.1)
    splitted <- split(ggplotdata,ggplotdata$variable)
    series <- foreach(group = names(splitted),.combine=append) %do% {
      wellData <- splitted[[group]]
      if(nrow(wellData) == 0) return(NULL)
      list(
          list(
               name=group,
               type='scatter',
               #data=toJSONArray2(wellData,json=FALSE)#,
               data=foreach(i=1:nrow(wellData)) %do% {
                 return(c(wellData$Time[i],wellData$value[i]))
               }
               #tooltip=
          ),
          list(
               name=group,
               type='errorbar',
               data=foreach(i=1:nrow(wellData)) %do% {
                 return(c(wellData$Time[i],c(wellData$value[i]-wellData$se[i],
                                             wellData$value[i]+wellData$se[i])))
               }
          )
      )
    }
    p$series(series)
    return(p)
  })

  plots[["growth rate vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=val,y=value,colour=label,group=variable))
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    }
    p <- p+geom_point(position=pd)+geom_line(position=pd)+
      guides(colour=guide_legend(title="Well",nrow=20))+
      scale_y_continuous(limits=c(-0.1,NA))+
      xlab(paste0("log ",input$label))+
      ylab("growth rate")
    return(p)
  })

  charts[["growth rate vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- rollingWindowRegression()
    p <- hPlot(x='val',y='value', data=ggplotdata, group='variable',type='scatter')
    p$chart(zoomType="xy")
    p$exporting(enabled=T)
    p$legend(layout='vertical',align="right",verticalAlign='top')
    colorNum <- length(WellsDesc())
    if(is.null(WellsDesc()))
      colorNum <- length(unique(ggplotdata$variable))
    p$colors(gg_color_hue(colorNum))
    p$plotOptions(scatter = list(lineWidth=1,marker=list(radius=8,symbol='circle')))
    p$xAxis(title=list(text=paste0("log ",input$label)))
    p$yAxis(title=list(text="growth rate"),floor=-0.1)
 
    #if(input$errorBars) {
    #  p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    #}
    return(p)
  })


  grToDt <- function(x) {return(log(2)*60/x)}
  
  plots[["doubling time vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- dtRegression()
  
    pd = position_dodge(0.1)
    p <- simplePlot("doubling time [min]",ggplotdata)
    if(input$errorBars) {
      p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    }    
    p <- p+ scale_y_continuous(limits=c(-10,as.numeric(input$maxdtime)))+
      scale_x_continuous(limits=timeLimits())
    return(p)  
  })

  charts[["doubling time vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- dtRegression()
  
    p <- simpleChart("",ggplotdata,WellsDesc())
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    p$yAxis(title=list(text="doubling time [min]"),floor=-10,ceiling=as.numeric(input$maxdtime))
    return(p)
    #if(input$errorBars) {
    #  p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    #}    
    #p <- p+ scale_y_continuous(limits=c(-10,as.numeric(input$maxdtime)))+
  })

  plots[["doubling time vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- dtRegression()
    
    pd = position_dodge(0.1)
    p <- ggplot(data=ggplotdata,aes(x=val,y=value,colour=label,group=variable))
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
  
  charts[["doubling time vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    ggplotdata <- dtRegression()

    p <- hPlot(x='val',y='value', data=ggplotdata, group='variable',type='scatter')
    p$chart(zoomType="xy")
    p$exporting(enabled=T)
    p$legend(layout='vertical',align="right",verticalAlign='top')
    colorNum <- length(WellsDesc())
    if(is.null(WellsDesc()))
      colorNum <- length(unique(ggplotdata$variable))
    p$colors(gg_color_hue(colorNum))
    p$plotOptions(scatter = list(lineWidth=1,marker=list(radius=8,symbol='circle')))
    p$xAxis(title=list(text=paste0("log ",input$label)))
    p$yAxis(title=list(text="doubling time [min]"),floor=-10,ceiling=as.numeric(input$maxdtime))

    #if(input$errorBars) {
    #  p <- p+geom_errorbar(aes(ymax=ymax,ymin=ymin),width=.1,position=pd)
    #}    
      #scale_y_continuous(limits=c(-10,as.numeric(input$maxdtime)))+
    return(p)  
  })
})
