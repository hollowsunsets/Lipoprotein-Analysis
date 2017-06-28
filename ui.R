library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
      h3("File Selection"),
      fileInput("sparklink", "Choose Sparklink File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("scans", "Choose Scans File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("amplog", "Choose Amplog File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx")),
      conditionalPanel(   # only appears if scan data has been provided 
        condition = "input.sparklink != null",
        h3("Scan Data Interactions"),
        selectInput("scansToRemove", label = h4("Remove a Scan"),
                    choices = list(list.from.scans.dataset.indicating.graphs.also.none),
                    selected = "None"), 
        selectInput("scansToAdd", label = h4("Add a Scan"),
                    choices = list(list.from.graph.changes.R),
                    selected = "None"),
        downloadButton('averageScans', "Download Average Scans")
      )),
    mainPanel(
      conditionalPanel(  # only appears if scan data has been provided
        condition = "input.sparklink != null",
        plotOutput("scanPlot")
      ),
      conditionalPanel(  # only appears if amplog data has been provided
        condition = "input.amplog != null",
        plotOutput("ampPlot")
      )
    )
  )
))