library(shiny)
shinyUI(fluidPage(
  titlePanel("Plate Reader data analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile","Excel reader data file:"),
      conditionalPanel(
        condition="input.datafile",
        #make the following conditional on having a file.
        htmlOutput("labelSelect"),
        #multiple choices of mode: whole plate, wells by row, wells by column, single well
        #Data to display: raw, log(raw) (requires input of background according to possibilites), growth rate, doubling time.
        numericInput("background","Background value:",min = 0, max = 1, step = 0.001,value = 0),
        textInput("well","Well to display",value = "A1")
        )
      ),
    mainPanel(
      plotOutput("mainPlot")
      )
    )
  ))