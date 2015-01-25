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
          choices = c("Whole plate","Row","Column","Wells"),
          selected = 1),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Wells"',
          textInput("wells","Wells to analyse",value="A1")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Row"',
          htmlOutput("rowSelect")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Column"',
          htmlOutput("columnSelect")
        ),
        selectInput(
          inputId = "analysisType",
          label = "Data to display",
          choices = c("Raw measurements","Growth rate analysis"),
          selected = 1
          ),
        conditionalPanel(
          condition='input.analysisType == "Growth rate analysis"',
          selectInput(
            inputId = "backgroundMethod",
            label = "Background subtraction method",
            choices = c("Average of first measurements of well",
                        "Time average of blank wells",
                        "Point-wise average of blank wells",
                        "Manual value"),
            selected = 1
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Manual value"',
            textInput("manualBackground","Background value to use",value="0.0")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Time average of blank wells"',
            textInput("averageBlanks","Blank wells to use",value="A1")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Point-wise average of blank wells"',
            textInput("pointWiseBlanks","Blank wells to use",value="A1")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Average of first measurements of well"',
            textInput("perWellMesNum","Number of measurements to use",value="1")            
            ),
          textInput("windowSize","Growth rate window size",value="5")                      
          )
        )
      ),
    mainPanel(
      conditionalPanel(
        condition='input.analysisType == "Raw measurements"',
        plotOutput("rawPlot")
        ),
      conditionalPanel(
        condition='input.analysisType == "Growth rate analysis"',
        plotOutput("logPlot"),
        plotOutput("growthRatePlot"),
        plotOutput("doublingPlot")
        )
      )
    )
  ))