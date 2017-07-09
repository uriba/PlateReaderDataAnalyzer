#ToDo:
#Add update plots button?
#Color code input types + collapsable for cleaner UI.

#Add plots for enzyme activity analysis:
## running window on background subtracted data

#Add plots for expression levels:
## selection of channels
## selection of background method per channel
## available graphs: fluorescence / od
## fluorescence change / od change (where the cells aim to)
## fluorescence change / od (rate of production).

#add plot.ly integration:
##Integrate plot.ly as choice for output for export (given username and authentication key).
##find way to use multiple columns in plot.ly legend.

#Set clear headlines for page according to analysis types
#Support multiple plate layout styles/files
#Add progress bar for plots

library(shiny)
library(gdata)
library(reshape2)
library(zoo)
library(rCharts)
library(foreach)
library(DT)
library(rhandsontable)

source('tecanProcess.R')

max_plots <- 10
addResourcePath("sample","sample")

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
    if(df[2,1] == 'A' && df[1,2]=='1') {
      print("explicit wells")
      rownames(df) <- df[,1]
      colnames(df) <- df[1,]
      df <- df[-1,-1]
    }
    else {
      datacols <- columns()
      datarows <- rows()
      if(length(colnames(df)) == length(datacols) || length(rownames(df)) == length(rows)) {
        colnames(df) <- datacols
        rownames(df) <- datarows
      }
      else {
        df <- t(df)
        colnames(df) <- datacols
        rownames(df) <- datarows
      }
    }
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
    if(input$wellsToAnalyse == "Match") {
      if(is.null(input$match)) { return() }
      wellsdesc <- layout()
      if(is.null(wellsdesc)) {
        cols <- cols[grep(input$match,cols)]
      } else {
        cols <- names(wellsdesc[grep(input$match,wellsdesc)])
      }
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
    minval <- 0.00001
    labelData <- Data()[[input$label]]
    cols <- wells()
    wellsData <- labelData[,wells(),drop=FALSE]
    blankVals <- backgroundVals()
    for(col in cols) {
      if(col %in% names(blankVals)) {
        wellsData[,col] <- pmax(wellsData[,col]-as.numeric(blankVals[col]),minval)
      } else {
        wellsData[,col] <- pmax(wellsData[,col]-blankVals,minval)
      }
    }      
    wellsData[,"Time"] = labelData$Time
    return(signif(wellsData[,c("Time",wells())],3))
  })
  
  backgroundSubtractedLog <- reactive({
    bgSubtracted <- backgroundSubtracted()
    bgSubtracted[,wells()] <- lapply(bgSubtracted[,wells(),drop=FALSE],log)
    return(signif(bgSubtracted,3))
  })  
  
  timeLimits <- reactive({
    labelData <- Data()[[input$label]]
    timesRange <- range(labelData$Time)
    delta <- timesRange[2]-timesRange[1]
    return(c(timesRange[1]-0.05*delta,timesRange[2]+0.05*delta))
  })
  
  wideRollingWindowRegression <- reactive({
    reg <- function(window) {
      cols <- colnames(window)
      cols <- cols[cols!="Time"]
      window <- as.data.frame(window)
      res <- list()
      for(col in cols) {
        stderrname <- paste0(col,'.std_err')
        data <- window[,col]
        times <- window[,"Time"]
        times <- times[is.finite(data)]
        data <- data[is.finite(data)]
        res[paste0(col,'.vals')]<-  mean(data)
        res[col] <- NA
        res[stderrname]<- NA
        if(length(data)>2) {
          c <- lm(data ~ times)
          rsq = summary(c)$r.squared
          if(!is.na(rsq) && rsq > as.numeric(input$rsquare)) {
            res[col] <- (summary(c)$coefficients)["times","Estimate"]
          }
          res[stderrname]<-summary(c)$coefficients["times","Std. Error"]
          res[ymin(col)]<-as.numeric(res[col])-as.numeric(res[stderrname])
          res[ymax(col)]<-as.numeric(res[col])+as.numeric(res[stderrname])
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
    return(regs)
   })

  wideDTRegression <- reactive({
    plotData <- wideRollingWindowRegression()
    plotData[,c(grep("vals",colnames(plotData)))] <- lapply(plotData[,c(grep("vals",colnames(plotData)))],exp)
    for(well in wells()) {
      stderr <- paste0(well,'.std_err')
      dts <- as.numeric(lapply(plotData[,well],grToDt))
      maxs <- as.numeric(lapply(as.numeric(plotData[,well])+as.numeric(plotData[,stderr]),grToDt))-dts
      mins <- dts - as.numeric(lapply(plotData[,well]-plotData[,stderr],grToDt))
      plotData[,well] <- dts
      plotData[,stderr] <- abs(maxs+mins)/2
    }
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

  rows <- reactive({
    if(is.null(Data())) { return() }
    plotData <- Data()[[names(Data())[1]]]
    return(levels(factor(substring(colnames(plotData)[-1],1,1))))
  })

  columns <- reactive({
    if(is.null(Data())) { return() }
    plotData <- Data()[[names(Data())[1]]]
    return(levels(factor(substring(colnames(plotData)[-1],2))))
  })

  output$rowSelect <- renderUI({
    if(is.null(Data())) { return() }
    selectInput(
      inputId = "row",
      label = "Select row to display",
      choices = rows(),
      selected = 1)
  })
  
  output$columnSelect <- renderUI({
    if(is.null(Data())) { return() }
    selectInput(
      inputId = "column",
      label = "Select column to display",
      choices = columns(),
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
                         downloadButtonName <- paste0("download",i)
                         if(is.null(input[[plotstylename]]) || input[[plotstylename]] == "plot") {
                             plot = showOutput(paste0("chart",i),"highcharts")
                         }
                         else if(input[[plotstylename]] == "table") {
                             plot = DT::dataTableOutput(paste0("table",i))
                           }
      tagList(uiOutput(plotstylename),downloadButton(downloadButtonName,'Download data'),plot)
    })
    do.call(tagList,plots_list)    
  })

  rawChart <- function(label,plotData,wellsDesc) {
      return(wideSeriesChart(plotData,"time [h]",label,NULL,NULL))
  }

   for (i in 1:max_plots) {
     local({
       my_i <- i
      plotstylename <- paste0("plotstyle",my_i)
      output[[plotstylename]] <- renderUI({
        selectInput(
                inputId = plotstylename,
                label = "Display",
                choices = c("plot","table"),
                selected = input[[plotstylename]])})
     })
   }

  simpleTableSketch <- function(data) {
    wellsDesc <- layout()
    wellsDesc <- wellsDesc[wells()]
    labels <- unique(wellsDesc)
    cols <- c()
    for(l in labels) {
      cols <- append(cols,names(wellsDesc[wellsDesc == l]))
    }
    sketch <- htmltools::withTags(table(class='display',
                                        thead(
                                             tr(
                                                th(rowspan=2,"Time"),
                                                lapply(labels,function(x) {
                                                       return(th(x,colspan=length(wellsDesc[wellsDesc == x])))
                                                })
                                             ),
                                             tr( lapply(cols,th))
                                        )))
    return(sketch)
   }

  structuredTableSketch <- function(data,label,stat) {
    cols <- wells()
    wellsDesc <- layout()
    noLabels <- is.null(wellsDesc)
    if(!noLabels) {
      wellsDesc <- wellsDesc[wells()]
      labels <- unique(wellsDesc)
      cols <- c()
      for(l in labels) {
        cols <- append(cols,names(wellsDesc[wellsDesc == l]))
      }
    }
    trows <- if(!noLabels) {
      htmltools::withTags(
                    list(
                   tr(
                      th(rowspan=3,"Time"),
                      lapply(labels,function(x) {
                             return(th(x,colspan=3*length(wellsDesc[wellsDesc == x])))
                      })
                   ),
                   tr( lapply(cols,function(x) {
                              return(th(x,colspan=3))
                      }))))}
             else {
               htmltools::withTags(tr(
                       th(rowspan=2,"Time"),
                       lapply(cols,function(x) {return(th(x,colspan=3))}
             )))}
    ls <- rep(c(label,stat,'StdErr'),length(cols))
    tlast <- htmltools::withTags(lapply(ls,th))
    sketch <- htmltools::withTags(table(class='display',
                                        thead(trows,tr(lapply(ls,th)))
                                        ))
    return(sketch)
  }

  output$layoutTable <- renderRHandsontable({
    data <- data.frame(row.names = rows())
    for (col in columns())
      for (row in rows()) {
        data[row,col] <- paste0(row,col)
        if(! is.null(WellsDesc())) {
          data[row,col] <- layout()[paste0(row,col)]
        }
      }
    data = data[,as.character(sort(as.numeric(colnames(data))))]
    return(rhandsontable(data, readOnly = FALSE))
  })

  layout <- function()
  {
    wellsDesc <- tableToDesc(input$layoutTable$params$data)
    if(is.null(wellsDesc))
       wellsDesc <- WellsDesc()
    return(wellsDesc)
  }

  tableToDesc <- function(desc) {
    if(is.null(desc)) return(NULL)
    ret <- c()
    cols = sort(as.numeric(columns()))
    for(i in 1:length(rows()))
      for(j in 1:length(cols))
        ret[paste0(rows()[i],cols[j])] <- desc[[i]][[j]]
    return(ret)
  }

  for (i in 1:max_plots) { # generate plots for all the labels (up to 10 labels are allowed)
    local({
      my_i <- i
      output[[paste0("chart",my_i)]] <- renderChart({
        if(is.null(wells())) { return() }
        if(input$analysisType == "Plate overview") {
          if(my_i>length(names(Data()))) {return ()}
          label <- names(Data())[my_i]
          plotData <- Data()[[label]]    
          p <- rawChart(label,plotData[,c("Time",wells())],layout())
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

      output[[paste0("table",my_i)]] <- DT::renderDataTable({
        if(is.null(wells())) { return() }
        if(input$analysisType == "Plate overview") {
          if(my_i>length(names(Data()))) {return ()}
          label <- names(Data())[my_i]
          data <- Data()[[label]][,c("Time",wells())]
          data <- signif(data,3)
          caption <- paste0(label," values over time")
          if(!is.null(layout())) {
            sketch <- simpleTableSketch(data)
            d <- datatable(data, container = sketch,rownames = FALSE,caption = caption)
          } else {
            d <- datatable(data,rownames = FALSE,caption = caption)
          }
          return(d)
        }
        else if(input$analysisType =="Growth rate analysis") {
          if(my_i>length(input$grPlots)) {return ()}
          return(tables[[input$grPlots[my_i]]]())
        }
      })

      output[[paste0("download",my_i)]] <- downloadHandler(
        filename = function() {paste0(caption(my_i),'.csv')},
        content = function(file) {
          if(input$analysisType == "Plate overview") {
            if(my_i>length(names(Data()))) {return ()}
            label <- names(Data())[my_i]
            data <- Data()[[label]][,c("Time",wells())]
          }
          else if(input$analysisType =="Growth rate analysis") {
            if(my_i>length(input$grPlots)) {return ()}
            data <- dataframe[[input$grPlots[my_i]]]()
          }
          write.csv(data,file)
        })
    })  
  }

  caption <- function(i) {
    filenames = c("raw" = 'raw',"background subtracted" = "background subtracted",
                  "background subtracted log"= "background subtracted log",
                  "growth rate vs. time" = "growth rate", "doubling time vs. time" = "doubling time",
                  "growth rate vs. value" = "growth rate", "doubling time vs. value" = "doubling time"
                  )
    if(input$analysisType == "Plate overview") {
      return(names(Data())[i])
    } else if(input$analysisType =="Growth rate analysis") {
      return(filenames[input$grPlots[i]])
    }
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

  simpleTable <- function(data,caption) {
    wellsDesc <- layout()
    if(!is.null(wellsDesc)) {
      sketch <- simpleTableSketch(data)
      return(datatable(data,container = sketch,rownames=FALSE,caption = caption))
    } else {
      return(datatable(data,rownames=FALSE,caption = caption))
    }
  }

  wideSeriesChart <- function(plotData,xlabel,ylabel,xlimits,ylimits,xsuffix = NULL) {
    wellnames <- wells()
    seriestitles <- wellnames
    labels <- wellnames
    wellsDesc <- layout()
    if(! is.null(wellsDesc)) {
      labels <- wellsDesc[wellnames]
      seriestitles <- lapply(wellnames,function(x) {paste(x, wellsDesc[x],sep=" - ")})
    }
    names(seriestitles) <- wellnames
    names(labels) <- wellnames
    groups = unique(labels)
    colorNum <- length(groups)

    p <- Highcharts$new()
    p$chart(zoomType="xy")
    p$exporting(enabled=T)
    p$legend(layout='vertical',align="right",verticalAlign='top')
    colors = gg_color_hue(colorNum)
    p$xAxis(title=list(text=xlabel),floor=xlimits[1],ceiling=xlimits[2])
    p$yAxis(title=list(text=ylabel),floor=ylimits[1],ceiling=ylimits[2])
    p$plotOptions(scatter = list(lineWidth=1,marker=list(radius=2,symbol='circle')))
    series <- foreach(group = wellnames,.combine=append) %do% {
      wellData <- plotData[[group]]
      if(is.null(xsuffix)) {
        xs <- plotData$Time
      } else {
        xs <- plotData[[paste0(group,xsuffix)]]
      }
      idx <- is.finite(wellData)
      xs <- xs[idx]
      wellData <- wellData[idx]
      if(length(wellData) == 0) return(NULL)
      append(
        list(
            list(
                 name=as.character(seriestitles[group]),
                 type='scatter',
                 color=colors[groups == labels[group]],
                 data=foreach(i=1:length(wellData)) %do% {
                   return(c(xs[i],wellData[i]))
                 }
            )),if(input$errorBars){
        mins <- plotData[[ymin(group)]]
        mins <- mins[idx]
        maxs <- plotData[[ymax(group)]]
        maxs <- maxs[idx]
        list(
          list(
               name=as.character(seriestitles[group]),
               type='errorbar',
               data=foreach(i=1:length(wellData)) %do% {
                 return(c(xs[i],c(mins[i],maxs[i])))
               }
          ))} else NULL
      )
    }
    p$series(series)
    return(p)
  }

  plots <- list()
  charts <- list()
  tables <- list()
  dataframe <- list()

  dataframe[["raw"]] <- reactive({
    if(is.null(wells())) { return() }
    label <- input$label
    data <- Data()[[label]]
    data <- signif(data[,c("Time",wells())],3)
  })

  charts[["raw"]] <- reactive({
    if(is.null(wells())) { return() }
    return(rawChart(input$label,dataframe[["raw"]](),layout()))
  })

  tables[["raw"]] <- reactive({
      caption = paste0(input$label," values over time")
      return(simpleTable(dataframe[["raw"]](),caption))
  })

  dataframe[["background subtracted"]] <- reactive({
    if(is.null(wells())) { return() }
    return(backgroundSubtracted())
  })

  charts[["background subtracted"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtracted()
    p <-rawChart(input$label,wellsData,layout())
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    return(p)
  })


  tables[["background subtracted"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtracted()
    caption <- paste0("Background subtracted ",input$label)
    caption <- paste0(caption," over time")
    return(simpleTable(wellsData,caption))
  })

  dataframe[["background subtracted log"]] <- reactive({
    if(is.null(wells())) { return() }
    return(backgroundSubtractedLog())
  })

  charts[["background subtracted log"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtractedLog()
    p <- rawChart(paste0("log ",input$label),wellsData,layout())
    limits <- timeLimits()
    p$xAxis(title=list(text="time [h]"),floor=limits[1],ceiling=limits[2])
    return(p)
  })

  tables[["background subtracted log"]] = reactive({
    if(is.null(wells())) { return() }
    wellsData <- backgroundSubtractedLog()
    is.na(wellsData) <- do.call(cbind,lapply(wellsData,is.infinite))
    caption <- paste0("Background subtracted log ",input$label)
    caption <- paste0(caption," over time")
    return(simpleTable(wellsData,caption))
  })

  growthRateData <- reactive({
    if(is.null(wells())) { return() }
    data <- wideRollingWindowRegression()
    is.na(data) <- do.call(cbind,lapply(data,is.infinite))
    cols <- colnames(data)
    data <- data[c("Time",cols[cols != "Time"])]
    data[,c(grep("vals",colnames(data)))] <- lapply(data[,c(grep("vals",colnames(data)))],exp)
    return(signif(data,3))
  })

  dataframe[["growth rate vs. time"]] <- growthRateData

  charts[["growth rate vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    plotdata <- wideRollingWindowRegression()
    return(wideSeriesChart(plotdata,"time [h]","growth rate",timeLimits(),c(-0.1)))
  })

  growthRateTable = reactive({
    if(is.null(wells())) { return() }
    data <- growthRateData()
    sketch <- structuredTableSketch(data,input$label,'Growth')
    caption <- paste0("Growth rate based on ",input$label)
    return(datatable(data,container = sketch,rownames=FALSE,caption = caption))
  })

  tables[["growth rate vs. time"]] = growthRateTable

  dataframe[["growth rate vs. value"]] <- growthRateData

  charts[["growth rate vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    plotdata <- wideRollingWindowRegression()
    return(wideSeriesChart(plotdata,paste0("log ",input$label),"growth rate",NULL,c(-0.1),xsuffix = ".vals"))
  })

  tables[["growth rate vs. value"]] = growthRateTable
  
  grToDt <- function(x) {return(log(2)*60/x)}

  ymax <- function(well) {
    return(paste0(well,".ymax"))
  }

  ymin <- function(well) {
    return(paste0(well,".ymin"))
  }

  grToDtFrame <- function(x) {
    for(well in wells()) {
      x[,well] <- as.numeric(lapply(x[,well],grToDt))
      ymax <- ymax(well)
      ymin <- ymin(well)
      if(ymax %in% colnames(x)) {
        x[,ymax] <- as.numeric(lapply(x[,ymax],grToDt))
        x[,ymin] <- as.numeric(lapply(x[,ymin],grToDt))
      }
    }
    return(x)
  }
  
  doublingTimeData <- reactive ({
    if(is.null(wells())) { return() }
    data <- wideDTRegression()
    is.na(data) <- do.call(cbind,lapply(data,is.infinite))
    cols <- colnames(data)
    data <- data[c("Time",cols[cols != "Time"])]
    return(signif(data,3))
  })

  dataframe[["doubling time vs. time"]] = doublingTimeData

  charts[["doubling time vs. time"]] = reactive({
    if(is.null(wells())) { return() }
    plotdata <- grToDtFrame(wideRollingWindowRegression())
    return(wideSeriesChart(plotdata,"time [h]","doubling time [min]",timeLimits(),c(-10,as.numeric(input$maxdtime))))
  })

  doublingTimeTable <- reactive({
    data <- doublingTimeData()
    sketch <- structuredTableSketch(data,input$label,'Doubling time')
    caption <- paste0("Doubling time [min] based on ",input$label)
    return(datatable(data,container = sketch,rownames=FALSE,caption = caption))
  })

  tables[["doubling time vs. time"]] = doublingTimeTable

  dataframe[["doubling time vs. value"]] = doublingTimeData

  charts[["doubling time vs. value"]] = reactive({
    if(is.null(wells())) { return() }
    plotdata <- grToDtFrame(wideRollingWindowRegression())
    return(wideSeriesChart(plotdata,paste0("log ",input$label),"doubling time [min]",NULL,c(-10,as.numeric(input$maxdtime)),".vals"))
  })

  tables[["doubling time vs. value"]] = doublingTimeTable
})
