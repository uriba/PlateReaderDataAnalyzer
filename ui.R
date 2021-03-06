library(shiny)
library(shinyBS)
library(DT)
library(rhandsontable)
require(rCharts)

shinyUI(fluidPage(
  titlePanel("Plate Reader Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      bsTooltip(id="datafile",title="Currently supporting Excel files generated by Tecan readers only. Uploading a single file will treat it as a series of measurements in time. Uploading multiple files will assume each file represents a single measurement cycle. In this case the number of plates must be specified and files will be read in an interleaved manner, assuming the files cycle through the plates",placement="right", trigger="hover"),
      fileInput("datafile","Excel reader data file/s:",multiple=TRUE),
      conditionalPanel(
        condition="output.MultiFile",
        textInput("platesnum","Number of plates",value=""),
        textInput("platenum","Plate to analyze",value="1")
      ),
      conditionalPanel(
        condition="output.fileUploaded",
        bsTooltip(id="descfile",title="Upload optional plate description file (Excel) to label wells",placement="right", trigger="hover"),
        fileInput("descfile","Plate description file"),
        bsTooltip(id="wellsToAnalyse",title="Which wells to display data on. Restricting the analysis to fewer wells increases the site's responsiveness",placement="right", trigger="hover"),
        selectInput(
          inputId = "wellsToAnalyse",
          label = "Wells to analyse",
          choices = c("Row","Column","Whole plate","Match","Wells"),
          selected = 1),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Wells"',
          bsTooltip(id="wells",title="Comma seperated, case sensitive list of one or more wells",placement="right", trigger="hover"),
          textInput("wells","Wells to analyse",value="A1")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Match"',
          textInput("match","Expression to search for wells to analyse",value="A1")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Row"',
          htmlOutput("rowSelect")
        ),
        conditionalPanel(
          condition='input.wellsToAnalyse == "Column"',
          htmlOutput("columnSelect")
        ),
        bsTooltip(id="analysisType",title="Plate overview displays all measurements vs. time. Growth rate analysis includes tools to analyze the growth rate",placement="right", trigger="hover"),
        selectInput(
          inputId = "analysisType",
          label = "Data to display",
          choices = c("Plate overview","Growth rate analysis","Plotly test"),
          selected = 1
          ),
        conditionalPanel(
          condition='input.analysisType == "Growth rate analysis" || input.analysisType == "Plotly test"',
          htmlOutput("labelSelect"),
          bsTooltip(id="labelSelect",title="Which measurement label should be used for the analysis",placement="right", trigger="hover"),
          bsTooltip(id="backgroundMethod",title="How the background level, needed for analysis of growth rate of each well should be determined. Options are: for each well, use the average of the first few measurements - good when uneven pipetting or different media is used in different wells, time average of blank wells allows the user to specify a list of wells the average of the values of which are used, point-wise average of blank wells will calculate the average of the blank wells at each time-point and subtract this value from the analyzed wells - good for measurements with fluctuations in time, manual value uses a user-supplied value as the background for analysis.",placement="bottom", trigger="hover"),
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
            bsTooltip(id="manualBackground",title="Use the supplied value as the background for all data points",placement="right", trigger="hover"),
            textInput("manualBackground","Background value to use",value="0.0")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Time average of blank wells" || input.backgroundMethod == "Point-wise average of blank wells"',
            bsTooltip(id="blankWells",title="Comma-seperated, case-sensitive list of wells to use for determining background values",placement="right", trigger="hover"),
            textInput("blankWells","Blank wells to use",value="A1")            
            ),
          conditionalPanel(
            condition='input.backgroundMethod == "Average of first measurements of well"',
            bsTooltip(id="perWellMesNum",title="Number of measurement points, starting at the first measurement, to take the average of and use as background value per well",placement="right", trigger="hover"),
            textInput("perWellMesNum","Number of measurements to use",value="1")            
            ),
          checkboxGroupInput("grPlots","Plots to display",c("raw",
                                                            "background subtracted",
                                                            "background subtracted log",
                                                            "growth rate vs. time",
                                                            "doubling time vs. time",
                                                            "growth rate vs. value",
                                                            "doubling time vs. value"),
                             selected=c("background subtracted log",
                                        "doubling time vs. time",
                                        "doubling time vs. value")),
          bsTooltip(id="windowSize",title="Number of measurement points to use in the running window determining the growth rate and doubling time",placement="right", trigger="hover"),
          textInput("windowSize","Growth rate window size",value="5"),
          checkboxInput("errorBars","Show error bars"),
          textInput("maxdtime","Maximum doubling time in minutes",value="300"),
          bsTooltip(id="rsquare",title="R-square threshold below which data points will not be displayed",placement="right", trigger="hover"),
          textInput("rsquare","R-square threshold",value="0.9")
          )
        )
      ),
    mainPanel(
      conditionalPanel(
        condition="! output.fileUploaded",
        includeHTML("welcome.html")
        ),
      conditionalPanel(
        condition="output.fileUploaded",
        tabsetPanel(
          tabPanel("About",includeHTML("welcome.html")),
          tabPanel("Data analysis",
             conditionalPanel(
              condition='output.fileUploaded && (input.analysisType == "Growth rate analysis")', 
              h3("Growth rate analysis")
              ),
            conditionalPanel(
              condition='output.fileUploaded && (input.analysisType == "Plate overview")', 
              h3("Plate overview")
              ),
            conditionalPanel(
              condition='(input.analysisType == "Plate overview") || (input.analysisType == "Growth rate analysis")', 
              htmlOutput("plots")
              ),
            conditionalPanel(
              condition='input.analysisType == "Plotly test"',
              showOutput("myChart","highcharts")
              )),
          tabPanel("Plate layout",
              rHandsontableOutput('layoutTable')
            ),
          id="tabDisplay",selected="Data analysis"
        )
    ))    
    )
  ))
