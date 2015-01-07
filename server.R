library(shiny)
library(gdata)
library(ggplot2)

valuesFromRow <- function(row) {
  row <- as.numeric(row)
  row <- row[!is.na(row)]
  return(row[-1])
}

labelRowsRange <- function(df,label) {
  labels <- as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1])
  labels[length(labels)+1] <- "End.Time:"
  nextLabel = labels[which(labels==label)+1]
  
  labelRow <-as.numeric(rownames(df[df[,1]==label,]))
  nextLabelRow <- as.numeric(rownames(df[df[,1]==nextLabel,]))
  return((labelRow+1):(nextLabelRow-1))
}

getPlateByLabel <- function(df,label) {
  return(df[labelRowsRange(df,label),])
}

transposePlateData <- function(pd) {
  pd <- as.data.frame(t())
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
    labels <- as.character(df[as.numeric(rownames(df[df[,1]=="Cycle Nr.",]))-1,1])
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
    firstcol <- plotData[,1]
    plotData <- as.data.frame(t(plotData))
    colnames(plotData) <- firstcol
    plotData <- plotData[-1,]
    plotData <- na.omit(plotData)
    #x <- as.matrix(plotData)
    #mode(x) <- "numeric"
    #plotData <- cbind(plotData,RTot=rowSums(x,na.rm=TRUE))
    
    colnames(plotData)[2] <- "Time"
    rownames(plotData) <- as.double(plotData$Time)
    
    #plotData[,"Time [s]"] <- as.numeric(plotData[,"Time [s]"])
    #plotData[,"A1"] <- as.numeric(plotData[,"A1"])
    #print(plotData[,"Time [s]"])
    #print(plotData[,"A1"])
    #plot(as.vector(x),as.vector(y))
    print(typeof(plotData$Time))
    #print(plotData$A1)
    p <- ggplot(aes(x=Time,y=A1),data=plotData)+geom_line()
    print(p)
  })
})