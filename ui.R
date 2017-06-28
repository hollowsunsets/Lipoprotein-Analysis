library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
      h3("File Selection"),
      fileInput("Sparklink", "Choose Sparklink File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("Scans", "Choose Scans File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      fileInput("Amplog", "Choose Amplog File",
                 accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx")),
      conditionalPanel(   # only appears if scan data has been provided 
        condition = "",
        selectInput("scans-to-remove", label = h4("Remove a Scan"),
                    choices = list(list.from.scans.dataset.indicating.graphs.also.none),
                    selected = "None"), 
        selectInput("scans-to-add", label = h4("Add a Scan"),
                    choices = list(list.from.graph.changes.R),
                    selected = "None"),
        downloadButton('average-scans', "Download Average Scans")
      )),
    mainPanel(
      conditionalPanel(  # only appears if scan data has been provided
        condition = "",
        plotOutput("scanPlot")
      ),
      conditionalPanel(  # only appears if amplog data has been provided
        condition = "",
        plotOutpu("nmPlot")
      )
    )
  )
))