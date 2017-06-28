library(shiny)

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
      uiOutput("scanLabels"),
      uiOutput("addScans"),
      uiOutput("removeScans"),
      uiOutput("averageScans")
    ),

    mainPanel(
      conditionalPanel(
        condition = "output.scanPlot", 
        plotOutput("scanPlot")
      ),
      conditionalPanel(
        condition = "output.ampPlot",
        plotOutput("ampPlot")
      )
    )
  )
))