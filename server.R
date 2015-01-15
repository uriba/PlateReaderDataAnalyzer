library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)

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
  
  output$fileUploaded <- reactive ({
    return (! is.null(Data()))
  })
  
  outputOptions(output,'fileUploaded',suspendWhenHidden=FALSE)

  output$labelSelect <- renderUI({ 
    if(is.null(Data())) { return() }
    selectInput(
    inputId = "label",
    label = "Select label to display",
    choices = names(Data()),
    selected = 1)
  })
  
  output$rowSelect <- renderUI({
    if(is.null(Data())) { return() }
    plotData <- Data()[[input$label]]
    rows <- levels(factor(substring(colnames(plotData)[-1],1,1)))
    selectInput(
    inputId = "row",
    label = "Select row to display",
    choices = rows,
    selected = 1)
  })
  
  output$columnSelect <- renderUI({
    if(is.null(Data())) { return() }
    plotData <- Data()[[input$label]]
    columns <- levels(factor(substring(colnames(plotData)[-1],2)))
    selectInput(
      inputId = "column",
      label = "Select column to display",
      choices = columns,
      selected = 1)
  })
  
  output$mainPlot <- renderPlot({   
    if(is.null(Data())) { return() }
    if(is.null(input$label)) { return() }
    
    label <- input$label
    plotData <- Data()[[input$label]]
    cols <- c(colnames(plotData))
    
    if(input$wellsToAnalyse == "Well") {
      if(is.null(input$well)) { return() }
      cols <- c("Time",input$well)
    }
    
    if(input$wellsToAnalyse == "Row") {
      if(is.null(input$row)) { return() }
      cols <- c("Time",colnames(plotData)[grep(input$row,colnames(plotData))])
    }
    
    if(input$wellsToAnalyse == "Column") {
      if(is.null(input$column)) { return() }
      cols <- c("Time",colnames(plotData)[grep(paste0("[A-Z]",input$column,"$"),colnames(plotData))])      
    }  
    plotData <- plotData[,cols]
  
    ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable))+geom_line()
  })
})
