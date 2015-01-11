library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)

labelSubset <- function(df,label) {
  labelRow <-as.numeric(rownames(df[df[,1]==label,]))
  fromLabel <- df[-(1:labelRow),]
  endRow <- as.numeric(rownames(fromLabel[fromLabel[,2]=="",]))[1]
  return(df[(labelRow+1):(endRow-1),])
}

getPlateByLabel <- function(df,label) {
  plotData <- labelSubset(df,label)
  firstcol <- plotData[,1] # The first column contains the descriptors of the rows
  plotData <- as.data.frame(t(plotData)) # Get the data to be in columns form
  colnames(plotData) <- firstcol # Set the proper column names
  plotData <- plotData[-1,] # Omit irrelevant columns and rows
  plotData <- na.omit(plotData)
  plotData[,1] = NULL
  plotData[,2] = NULL
  colnames(plotData)[1] <- "Time" #Rename the time column so that it has a "nicer" name
  plotData[] <- lapply(plotData,as.character) # Convert values to numeric
  plotData[] <- lapply(plotData,as.numeric)
  return(plotData)
}

getReaderData.1.8 <- function(df) {
  df[df=="OVER"] <- "71000"
  labels <- as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1]) #in this version labels are two lines above measurement tables
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
    if(version == 'Tecan i-control , 1.8.50.0') {
      return(getReaderData.1.8(df))
    }
    if(version == 'Tecan i-control , 1.11.1.0') {
      return(NULL)
    }
  })
  
  output$fileUploaded <- reactive ({
    return (! is.null(Data()))
  })
  
  outputOptions(output,'fileUploaded',suspendWhenHidden=FALSE)

  output$labelSelect <- renderUI({ selectInput(
    inputId = "label",
    label = "Select label to display",
    choices = names(Data()),
    selected = 1)
  })
  
  output$mainPlot <- renderPlot({   
    if(is.null(input$datafile)) { return() }
    label <- input$label
    plotData <- Data()[input$label]
    if(input$wellsToAnalyse == "Well") {
      cols <- c("Time",input$well)
      plotData <- plotData[,cols]
    }
    
    ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable))+geom_line()
  })
})
