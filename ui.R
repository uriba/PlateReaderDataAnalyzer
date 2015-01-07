library(shiny)
shinyUI(fluidPage(
  titlePanel("Plate Reader data analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile","Excel reader data file:"),
      htmlOutput("labelSelect"),
      numericInput("background","Background value:",min = 0, max = 1, step = 0.001,value = 0),
      textInput("well","Well to display",value = "A1")
      ),
    mainPanel(
      plotOutput("odPlot"),
      plotOutput("platePlot")
      )
    )
  ))