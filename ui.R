library(shiny)
require(rCharts)

shinyUI(fluidPage(
  titlePanel("Plate Reader Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile","Excel reader data file:"),
      conditionalPanel(
        condition="output.fileUploaded",
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
          choices = c("Plate overview","Growth rate analysis","Plotly test"),
          selected = 1
          ),
        conditionalPanel(
          condition='input.analysisType == "Growth rate analysis" || input.analysisType == "Plotly test"',
          htmlOutput("labelSelect"),
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
            condition='input.backgroundMethod == "Time average of blank wells" || input.backgroundMethod == "Point-wise average of blank wells"',
            textInput("blankWells","Blank wells to use",value="A1")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Average of first measurements of well"',
            textInput("perWellMesNum","Number of measurements to use",value="1")            
            ),
          checkboxGroupInput("grPlots","Plots to display",c("raw",
                                                            "background subtracted",
                                                            "background subtracted log",
                                                            "growth rate vs. time",
                                                            "doubling time vs. time",
                                                            "growth rate vs. value",
                                                            "doubling time vs. value")),
          textInput("windowSize","Growth rate window size",value="5"),
          checkboxInput("errorBars","Show error bars"),
          textInput("maxdtime","Maximum doubling time in minutes",value="300"),
          textInput("rsquare","R-square threshold",value="0.9")
          )
        )
      ),
    mainPanel(
      conditionalPanel(
        condition='(input.analysisType == "Plate overview") || (input.analysisType == "Growth rate analysis")', 
        htmlOutput("plots")
        ),
      conditionalPanel(
        condition='input.analysisType == "Plotly test"',
        showOutput("myChart","highcharts")
        )
      )    
    )
  ))
