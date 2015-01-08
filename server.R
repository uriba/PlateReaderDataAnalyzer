library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)

valuesFromRow <- function(row) {
  row <- as.numeric(row)
  row <- row[!is.na(row)]
  return(row[-1])
}
getLabels <- function(df) {
  return (as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1]))
}
labelRowsRange <- function(df,label) {
  labels <- getLabels(df)
  labels[length(labels)+1] <- "End.Time:"
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

getTimes <- function(df,label) {
  labelrow <-as.numeric(rownames(df[df[,1]==label,]))
  times <- df[labelrow+2,]
  return(valuesFromRow(times))
}

getWell <- function(df,well,label) {
  labelRow <- as.numeric(rownames(df[as.numeric(rownames(df[df[,1]==label,])),]))
  wellRows <- df[df[,1] == well,]
  if(length(rownames(wellRows))==0) { return () }
  wellRow <- wellRows[as.numeric(rownames(wellRows))>labelRow,][1,]
  return(valuesFromRow(wellRow))
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

  WellSeries <- reactive({
    if(is.null(input$datafile)) { return() }
    df <- Data()$df    
    label <- input$label
    if(label == "") { return() } 
    times <- as.numeric(getTimes(df,label))
    values <- getWell(df,input$well,label)
    return(list(times = times, values = values))
  })
  
  output$labelSelect <- renderUI({ selectInput(
    inputId = "label",
    label = "Select label to display",
    choices = Data()$labels,
    selected = 1)
  })
  
  output$odPlot <- renderPlot({
    if(is.null(WellSeries()$values)) { return() }
    times <- WellSeries()$times
    values <- WellSeries()$values
    label <- input$label
    plot(times,values-input$background,
         type = "l",main = input$well,
         xlab = "Time [sec]", ylab = label,
         ylim  = c(0,max(values)))
  })
  
  output$platePlot <- renderPlot({   
    if(is.null(input$datafile)) { return() }
    label <- input$label
    plotData <- getPlateByLabel(Data()$df,label)
    
    ggplotdata <- melt(plotData,id="Time") # Reformat the data to be appropriate for multi line plot
    ggplot(data=ggplotdata,aes(x=Time,y=value,colour=variable))+geom_line()
  })
})