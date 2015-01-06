library(shiny)
library(gdata)

valuesFromRow <- function(row) {
  row <- as.numeric(row)
  row <- row[!is.na(row)]
  return(tail(row,length(row)-1))
}

getTimes <- function(df,label) {
  labelrow <-as.numeric(rownames(df[df[,1]==label,]))
  times <- df[labelrow+2,]
  return(valuesFromRow(times))
}

getWell <- function(df,well,label) {
  labelRow <- as.numeric(rownames(df[as.numeric(rownames(df[df[,1]==label,])),]))
  wellRows <- df[df[,1] == well,]
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
    if(is.null(WellSeries())) { return() }
    times <- WellSeries()$times
    values <- WellSeries()$values
    label <- input$label
    plot(times,values-input$background,
         type = "l",main = input$well,
         xlab = "Time [sec]", ylab = label,
         ylim  = c(0,max(values)))
  })
  
  output$odLogPlot <- renderPlot({
    if(is.null(WellSeries())) { return() }    
    times <- WellSeries()$times
    values <- WellSeries()$values
    label <- input$label
    plot(times,values-input$background,
         type = "l",main = input$well,
         xlab = "Time [sec]", ylab = label,
         log = "y")
  })
})