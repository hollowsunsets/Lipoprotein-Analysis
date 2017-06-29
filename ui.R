library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Scan Visualizations"),
  sidebarLayout(
    sidebarPanel(
      h3("File Selection"),
      p("Please upload the data files that you wish to visualize using this application."),
      fileInput("sparklinkData", "Choose Sparklink File",
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
      fileInput("scansData", "Choose Scans File",
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
      fileInput("amplogData", "Choose Amplog File",
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv",".xlsx")),
      tags$hr(),
      uiOutput("scanPlotDone")
      
    ),

    mainPanel(
      conditionalPanel(
        condition = "output.scanPlotDone",
        selectInput("removeScans", label = h4("Remove a Scan"), 
                    choices = list(1, 2, 3, 4),
                    selected = "None"),
        selectInput("addScans", label = h4("Add a Scan"),
                    choices = list(1, 2, 3, 4),
                    selected = "None"),
        plotOutput("scanPlot")
      ),
      conditionalPanel(
        condition = "output.ampPlot",
        plotOutput("ampPlot")
      )
    )
  )
))