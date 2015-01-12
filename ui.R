library(shiny)
shinyUI(fluidPage(
  titlePanel("Plate Reader data analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile","Excel reader data file:"),
      conditionalPanel(
        condition="output.fileUploaded",
        #make the following conditional on having a file.
        htmlOutput("labelSelect"),
        #multiple choices of mode: whole plate, wells by row, wells by column, single well
        selectInput(
          inputId = "wellsToAnalyse",
          label = "Wells to analyse",
          choices = c("Whole plate","Row","Column","Well"),
          selected = 1),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Well"',
          textInput("well","Well to analyse",value="A1")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Row"',
          htmlOutput("rowSelect")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Column"',
          htmlOutput("columnSelect")
        )
        
        #Data to display: raw, log(raw) (requires input of background according to possibilites), growth rate, doubling time.
        #numericInput("background","Background value:",min = 0, max = 1, step = 0.001,value = 0)
        )
      ),
    mainPanel(
      plotOutput("mainPlot")
      )
    )
  ))