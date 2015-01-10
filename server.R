library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)

getLabels <- function(df) {
  return (as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1]))
}

labelRowsRange <- function(df,label) {
  labels <- getLabels(df)
  labels[length(labels)+1] <- "End Time:"
  nextLabel = labels[which(labels==label)+1]
  
  labelRow <-as.numeric(rownames(df[df[,1]==label,]))
  nextLabelRow <- as.numeric(rownames(df[df[,1]==nextLabel,]))
  return((labelRow+1):(nextLabelRow-1))
}

getPlateByLabel <- function(df,label) {
  plotData <- df[labelRowsRange(df,label),]
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

shinyServer(function(input,output) {
  Data <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    df <- read.xls(inFile$datapath,stringsAsFactors=FALSE)
    df[df=="OVER"] <- "71000"
    labels <- getLabels(df)
    return(list(df = df, labels = labels))
  })

  output$labelSelect <- renderUI({ selectInput(
    inputId = "label",
    label = "Select label to display",
    choices = Data()$labels,
    selected = 1)
  })
  
  output$odPlot <- renderPlot({
    if(is.null(input$datafile)) { return() }
    label <- input$label
    plotData <- getPlateByLabel(Data()$df,label)
    cols <- c("Time",input$well)
    plotData <- plotData[,cols]

    ggplotdata <- melt(plotData,id="Time")
    ggplot(data=ggplotdata,aes(x=Time,y=value))+geom_line()
  })
  
  output$mainPlot <- renderPlot({   
    if(is.null(input$datafile)) { return() }
    label <- input$label
    plotData <- getPlateByLabel(Data()$df,label)
    
    ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable))+geom_line()
  })
})